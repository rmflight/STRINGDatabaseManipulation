#' strip species id from identifier
#'
#' given a set of STRING ID's, remove the species taxonomy id part to get an ENSEMBL
#' protein ID.
#'
#' @param string_id character vector of STRING IDs (XXXX.ENSPXXX)
#'
#' @return character vector
#' @export
#'
strip_species <- function(string_id){
  substring(string_id, 6)
}


#' convert to graphBAM
#'
#' convert a STRING data.frame to a graphBAM graph for use in other applications
#'
#' @param string_data a data.frame of STRING links
#' @param use_weights a column of the data.frame to use for weights. If NULL, value of 1 is used.
#' @export
#' @return graphBAM graph
#' @import graph
string_2_graphBAM <- function(string_data, use_weights = NULL){
  stopifnot(is.data.frame(string_data))
  string_edges <- string_data[, c(1,2)]
  names(string_edges) <- c("from", "to")
  if (is.null(use_weights)) {
    string_edges$weight <- 1
  } else {
    if (use_weights %in% names(string_data)) {
      string_edges$weight <- string_data[[use_weights]]
    }
  }


  string_graph <- graph::graphBAM(string_edges, edgemode = "undirected", ignore_dup_edges = TRUE)
  string_graph
}

#' find links between nodes
#'
#' given a data frame of edges, find nodes within so many edges of initial nodes
#' with option that final set of edges go to known nodes
#'
#' @param in_graph a graphBAM graph
#' @param start_nodes which nodes to start from
#' @param n_hop how many hops to go out (default is 1)
#' @param end_nodes optional, only keep edges that end at these nodes
#' @param drop_same_after_2 should edges that only go to a single other node
#'   be dropped? Default is TRUE. See DETAILS for more information.
#'
#' @details One frequently encountered case will be a set of nodes that do:
#'   N1 --> N2 --> N1 (because the network is assumed to be undirected), where
#'   after a single hop from N1 we reach N2, and then a second hop returns to N1.
#'   Even in the case where the end_nodes are the same as the start_nodes, this
#'   is likely \emph{not} useful information. So \code{drop_same_after_1 = TRUE}
#'   will set the results so that these edge paths are not returned and kept
#'   in the final graph.
#'
#' @import graph
#' @export
#' @return list
#' @examples
#' library(STRINGDatabaseManipulation)
#' library(graph)
#' set.seed(1234)
#' link_data <- STRING10_links
#' link_data <- link_data[sample(nrow(link_data), 10000),]
#' link_graph <- string_2_graphBAM(link_data)
#' start_nodes <- sample(nodes(link_graph), 10)
#' end_nodes <- NULL
#' n_hop <- 3
#' find_edges(link_graph, start_nodes, n_hop)
#'
#' find_edges(link_graph, start_nodes, n_hop, end_nodes, drop_same_after_2 = FALSE)
find_edges <- function(in_graph, start_nodes, n_hop = 1, end_nodes = NULL, drop_same_after_2 = TRUE){

  stopifnot(class(in_graph) == "graphBAM")
  all_nodes <- nodes(in_graph)
  if (is.null(end_nodes)) {
    end_nodes <- all_nodes
  }

  query_nodes <- start_nodes

  edge_traverse <- matrix("", nrow = length(all_nodes), ncol = n_hop + 1)
  for (i_hop in seq_len(n_hop)){

    hop_edges <- edges(in_graph, query_nodes)
    if (i_hop == 1){

      to_edges <- unlist(hop_edges, use.names = FALSE)
      from_edges <- lapply(names(hop_edges), function(x){
        rep(x, length(hop_edges[[x]]))
      })
      from_edges <- unlist(from_edges, use.names = FALSE)
      edge_traverse <- cbind(from_edges, to_edges)
      query_nodes <- unique(to_edges)

      same_loc <- rep(FALSE, nrow(edge_traverse))

    } else {

      out_nodes <- lapply(names(hop_edges), function(x){
        node_loc <- which(edge_traverse[, i_hop] %in% x)
        to_edges <- hop_edges[[x]]
        n_edge <- length(to_edges)

        from_edges <- edge_traverse[rep(node_loc, n_edge), , drop = FALSE]
        to_edges <- rep(to_edges, each = length(node_loc))
        cbind(from_edges, to_edges)
      })
      tmp_traverse <- do.call(rbind, out_nodes)

      # Look for things that are already "", so we set them again
      null_index <- which(nchar(edge_traverse[, i_hop]) == 0)
      null_traverse <- edge_traverse[null_index, , drop = FALSE]
      if (length(null_index) > 0) {
        null_traverse <- cbind(null_traverse, "")
        tmp_traverse <- rbind(tmp_traverse, null_traverse)
      }

      # then work on the things identified before to be the same
      # we do this here because otherwise the logic doesn't flow, and we want
      # to be able to find things that loop back to themselves
      same_traverse <- edge_traverse[same_loc, , drop = FALSE]
      if (nrow(same_traverse) > 0) {
        same_traverse <- cbind(same_traverse, "")
        tmp_traverse <- rbind(tmp_traverse, same_traverse)
      }

      edge_traverse <- tmp_traverse
    }

    # as a way to stop early and not consider those edges that merely return to
    # the start after ONLY two hops (N1 --> N2 --> N1), set the second N1 to ""
    # so that this edge-path will not be considered at all in future hops, nor
    # will it be kept in the backtracking part
    if (drop_same_after_2 && (i_hop == 2)) {
      tmp_same <- edge_traverse[, 1] == edge_traverse[, i_hop + 1]
      edge_traverse[tmp_same, i_hop + 1] <- ""
    }

    # check for locations where last node is same as a previous node, and use this to remove things to search
    # for. In next round, will set to "". We do this because we want to potentially keep these traversals, but not
    # include them in any more rounds of searching.
    null_loc <- nchar(edge_traverse[, i_hop + 1]) == 0
    same_loc <- apply(edge_traverse, 1, function(x){
      sum(x[i_hop + 1] %in% x[1:i_hop]) > 0
    })
    same_loc <- same_loc & !(null_loc)
    query_nodes <- unique(edge_traverse[!same_loc, i_hop + 1])
    query_nodes <- query_nodes[!(nchar(query_nodes) == 0)]

  }

  # after creating the node matrix, find those instances where we hit the target nodes
  # by going backwards from the last hop to the first, saving those edge paths
  # where we encounter the target nodes
  keep_traverse <- rep(FALSE, nrow(edge_traverse))
  for (i_hop in seq(ncol(edge_traverse), 2, -1)) {
    keep_traverse <- keep_traverse | (edge_traverse[, i_hop] %in% end_nodes)
    edge_traverse[!keep_traverse, i_hop] <- ""
  }

  keep_nodes <- unique(as.vector(edge_traverse[keep_traverse, ]))
  keep_nodes <- keep_nodes[!(nchar(keep_nodes) == 0)]
  keep_nodes <- unique(keep_nodes)
  remove_nodes <- all_nodes[!(all_nodes %in% keep_nodes)]

  return(list(graph = removeNode(remove_nodes, in_graph), nodes = keep_nodes))
}

#' alternative find_edges from both
#'
#' a less general approach is to come from both the start and end nodes, go
#' one hop, and find the intersection of all pairwise comparisons. This method is
#' useful as it provides a simple check on the original find_edges method for 2
#' hops.
#'
#' @param in_graph a graphBAM
#' @param start_nodes the start nodes to use
#' @param end_nodes the end nodes to reach
#'
#' @return with new graph and all nodes in graph
#' @export
#'
#' @examples
#' library(STRINGDatabaseManipulation)
#' library(graph)
#' set.seed(1234)
#' link_data <- STRING10_links
#' link_data <- link_data[sample(nrow(link_data), 10000),]
#' in_graph <- string_2_graphBAM(link_data)
#' start_nodes <- end_nodes <- sample(nodes(link_graph), 100)
#' out_graph <- find_intersecting_nodes(in_graph, start_nodes, end_nodes)
find_intersecting_nodes <- function(in_graph, start_nodes, end_nodes){
  stopifnot(class(in_graph) == "graphBAM")

  adj_start <- adj(in_graph, start_nodes)
  adj_end <- adj(in_graph, end_nodes)

  all_comparisons <- expand.grid(start_nodes, end_nodes, stringsAsFactors = FALSE)

  keep_nodes <- lapply(seq(1, nrow(all_comparisons)), function(in_row){
    n1 <- all_comparisons[in_row, 1]
    n2 <- all_comparisons[in_row, 2]

    out_nodes <- intersect_nodes <- character(0)

    if (n1 != n2) {
      intersect_nodes <- base::intersect(c(n1, adj_start[[n1]]), c(n2, adj_end[[n2]]))
    }

    if (length(intersect_nodes) != 0) {
      out_nodes <- c(n1, n2, intersect_nodes)
    }

    return(out_nodes)
  })

  keep_nodes <- unique(unlist(keep_nodes))

  all_nodes <- nodes(in_graph)
  remove_nodes <- all_nodes[!(all_nodes %in% keep_nodes)]

  return(list(graph = removeNode(remove_nodes, in_graph), nodes = keep_nodes))

}

#' find shortest path
#'
#' finds the nodes that make a shortest path between two given sets of nodes
#' and a distance paramter. This is done by breaking any edges between the sets of
#' nodes provided, and then asking for the shortest path from a node in one set to
#' the nodes in the other set, trimming to those that are within a specific number
#' of hops.
#'
#' This function should use an igraph based graph, and then use some of the functionality
#' in all_shortest_paths
find_edges_shortest_path <- function(){

}
