#' oar
#'
#' Description:
#' A look-up table based on tiling the plane with oriented squares,
# where 'oriented' means vertices of each square are labeled clockwise
# starting at 6:00. Rows of the table are the labels of the vertices of
# adjacent edges of these squares.
#'
#'
#' @format A data frame with 4 rows and 4 variables:
#' \describe{
#'  | a | b | c | d |
#'  |:-:|:-:|:-:|:-:|
#'  | 1 | 2 | 4 | 3 |
#'  | 2 | 3 | 1 | 4 |
#'  | 3 | 4 | 2 | 1 |
#'  | 4 | 1 | 3 | 2 |
#' }
#'
#' @docType data
#'
#' @usage data(oar)
#'
#' @source Source
"oar"
