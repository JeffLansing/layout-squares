test_that("depth_first", {
  graph <- rgraph6::igraph_from_text("KP@?Y_AG?C?G")[[1]]
  vec <- c(1,5,4,6,11,3,2,8,10,9,7)
  expect_identical(depth_first_gr(graph), vec)
})

test_that("get_evids", {
  vnames <- 1:14 %>% as.character()
  graph <- rgraph6::igraph_from_text("KP@?Y_AG?C?G")[[1]]
  mx <- c(1,3,3,4,8,8,2,6,7,7,7,3,10,4,8,12,2,6,7,11,9,5) %>% matrix(ncol = 2)
  expect_identical(get_evids(graph, vnames), mx)
})
