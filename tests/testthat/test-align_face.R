test_that("align_face", {
  face <- c(3,1,1,4,1,0,10,0,0,9,0,1) %>%
    array(dim = c(3,4), dimnames = list(c('nm', 'x', 'y'), letters[1:4]))
  expected <- c(10,0,0,9,0,1,3,1,1,4,1,0) %>%
    array(dim = c(3,4), dimnames = list(c('nm', 'x', 'y'), letters[1:4]))
  expect_identical(align_face(face), expected)
})
