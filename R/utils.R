#' strip species id from identifier
#'
#' given a set of STRING ID's, remove the species taxonomy id part to get an ENSEMBL
#' protein ID.
#'
#' @param string_id character vector of STRING IDs (XXXX.ENSPXXX)
#'
#' @return character vector
#'
strip_species <- function(string_id){
  substring(string_id, 6)
}

#' find links between nodes
#'
#' given a data frame of edges, find nodes within so many edges of initial nodes
#' with option that final set of edges go to known nodes
#'
#' @param link_data a data.frame of links
#' @param start_nodes which nodes to start from
#' @param n_hop how many hops to go out (default is 1)
#' @param end_nodes optional, only keep edges that end at these nodes
#'
#' @import graph
#' @export
#' @return list
#' @examples
#' library(STRINGDatabaseManipulation)
#' set.seed(1234)
#' link_data <- STRING10_links
#' link_data <- link_data[sample(nrow(link_data), 10000),]
#' link_data <- link_data[, c(1,2)]
#' names(link_data) <- c("from", "to")
#' link_data$weight <- 1
#' start_nodes <- sample(unique(c(link_data[,1], link_data[,2])), 10)
#' end_nodes <- NULL
#' n_hop <- 3
#' find_edges(link_data, start_nodes, n_hop)
find_edges <- function(link_data, start_nodes, n_hop = 1, end_nodes = NULL){
  all_nodes <- unique(c(link_data[,1], link_data[,2]))
  if ("" %in% all_nodes) {
    stop('There is a node with a name of "", the empty string, which is not allowed.', call. = TRUE)
  }

  if (is.null(end_nodes)) {
    end_nodes <- all_nodes
  }

  link_graph <- graphBAM(link_data, edgemode = "undirected", ignore_dup_edges = TRUE)

  query_nodes <- start_nodes

  edge_traverse <- matrix("", nrow = length(all_nodes), ncol = n_hop + 1)
  for (i_hop in seq_len(n_hop)){

    hop_edges <- edges(link_graph, query_nodes)
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

    # check for locations where last node is same as first node, and use this to remove things to search
    # for. In next round, will set to NA. We do this because we want to potentially keep these traversals
    same_loc <- edge_traverse[, 1] == edge_traverse[, i_hop + 1]
    query_nodes <- unique(edge_traverse[!same_loc, i_hop + 1])
    query_nodes <- query_nodes[!(nchar(query_nodes) == 0)]

  }

  # after creating the node matrix, find those instances where we hit the target nodes
  keep_traverse <- rep(FALSE, nrow(edge_traverse))
  for (i_hop in seq(ncol(edge_traverse), 2, -1)){
    keep_traverse <- keep_traverse | (edge_traverse[, i_hop] %in% end_nodes)
    edge_traverse[!keep_traverse, i_hop] <- ""
  }

  keep_nodes <- unique(as.vector(edge_traverse[keep_traverse, ]))
  keep_nodes <- keep_nodes[!(nchar(keep_nodes) == 0)]
  remove_nodes <- all_nodes[!(all_nodes %in% keep_nodes)]

  return(list(graph = removeNode(remove_nodes, link_graph), nodes = keep_nodes))
}
