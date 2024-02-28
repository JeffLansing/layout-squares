#' d8
#'
#' Description.
#'
#' @format A data frame with 8 rows and 4 variables:
#' \describe{
#' e = c(1, 2, 3, 4), # ()         noop
#' a2 = c(3, 4, 1, 2), # (13)(24)  rotation by 180
#' a3 = c(4, 1, 2, 3), # (1432)    rotation by 270 counter-clockwise
#' a = c(2, 3, 4, 1), # (1234)     rotation by 90 counter-clockwise
#' a2x = c(1, 4, 3, 2), # (24)     reflection about the diagonal 1-3
#' x = c(3, 2, 1, 4), # (13)       reflection about the diagonal 2-4
#' ax = c(4, 3, 2, 1), # (14)(23)  reflection about the line joining midpoints of sides 14 and 23
#' a3x = c(2, 1, 4, 3)  # (12)(34) reflection about the line joining midpoints of sides 12 and 34
#' }
#' @source SageMath
#'
#' @docType data
#'
#' @usage data(d8)
#'
#' @examples
#' data(d8)
#' vec <- c(2, 3, 5, 7)
#' vec[d8[2,]][d8[2,]] == vec #expect TRUE
#'
"d8"
