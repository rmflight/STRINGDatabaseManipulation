context("Utilities")

test_that("STRING ids are trimmed properly", {
  test_string <- c("9606.ENSP00000360761", "9606.ENSP00000380184", "9606.ENSP00000264409",
                   "9606.ENSP00000317300", "9606.ENSP00000291572")
  expected_string <- c("ENSP00000360761", "ENSP00000380184", "ENSP00000264409", "ENSP00000317300",
                       "ENSP00000291572")

  expect_equal(expected_string, strip_species(test_string))
})

test_that("Correct graph is returned", {
  set.seed(1234)
  data("STRING10_links")
  link_data <- STRING10_links
  link_data <- link_data[sample(nrow(link_data), 10000),]
  in_graph <- string_2_graphBAM(link_data)
  start_nodes <- sample(unique(c(link_data[,1], link_data[,2])), 100)
  end_nodes <- start_nodes
  n_hop <- 2
  out_graph <- find_edges(in_graph, start_nodes, n_hop, end_nodes)

  # alternative method to verify (only works for 2 hop case and back to start nodes)
  out_graph2 <- find_intersecting_nodes(in_graph, start_nodes, end_nodes)
  expect_equal(out_graph$graph, out_graph2$graph)
})
