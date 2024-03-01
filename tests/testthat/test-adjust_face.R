test_that("adjust face", {
  far <- example_far
  rar <- example_rar
  expected  <- c(14,1,0,10,1,1,9,2,1,13,2,0) %>%
    array(dim = c(3,4), dimnames = list(c('nm', 'x', 'y'), letters[1:4]))
  expect_identical(adjust_face(far, rar, 1, 5)[,,5], expected)
})
