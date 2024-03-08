#' Apply an operation
#'
#' @description
#' Rotate the face with the operation, and then align it,
#'
#' @param face a face to transform
#' @param op a 3d rotation operator
#'
#' @return the transformed face
#' @export
#'
#' @examples
#' face <- array(c(7,0,0,4,0,1,10,1,1,14,1,0),dim = c(3,4),
#' dimnames = list(c('nm', 'x', 'y'), letters[1:4]))
#' apply_op(face, d8[4,])
#'
apply_op <- function(face, op) {
  if(is.null(face)) {
    stop("face is null")
  }
  face[nm,] <- face[nm,][op]
  face
}

#' select by u and v
#'
#' @description
#' SELECT * FROM rar WHERE (f1 = u AND f2 = v) OR (f1 = v AND f2 = u)
#'
#' @param rar a ridge array data structure
#' @param u an index of a face in r
#' @param v an index of another face in rar
#'
#' @return the row in rar that contains both faces
#' @export
#'
#' @examples
#' rar <- example_rar
#' selectby_uv(rar, 1,5)[3:4]
#'
selectby_uv <- function(rar, u, v) {
  m1 <- which(rar[,1:2] == u, arr.ind = TRUE)
  m2 <- which(rar[,1:2] == v, arr.ind = TRUE)
  rix <- intersect(m1[,1], m2[,1])
  rar[rix,]
}

#' Get other edge
#'
#' @param edge an edge of a face
#'
#' @return the adjoining edge of the adjoining face
#' @export
#'
#' @examples
#' xx <- c(1,4)
#' yy <- c(2,3)
#' identical(get_other_edge(xx), yy)
get_other_edge <- function(edge) {
  c(
    oar[oar[,1:2] %>% apply(1, function(rw) {all(edge == rw)}),3:4],
    oar[oar[,3:4] %>% apply(1, function(rw) {all(edge == rw)}),1:2]
  ) %>% as.numeric()
}

#' get_op for u, v
#'
#' @param far a face array data structure
#' @param rar a ridge array data structure
#' @param u in index of a face
#' @param v an index of an adjoining face
#'
#' @return the operator which, when applied to face v, aligns v with u
#' @export
#'
#' @examples
#' far <- example_far
#' rar <- example_rar
#' get_opuv(far, rar, 1, 3)
#'
get_opuv <- function(far, rar, u, v) {
  nu <- far[1,,u]
  vv <- selectby_uv(rar, u,v)[3:4] %>% as.integer()

  xx <- match(vv, nu)
  yy <- get_other_edge(xx)
  ops <- d8
  for(i in 1:8) {
    op <- ops[i,]
    nm <- (far[,,v] %>% apply_op(op))[1,]
    uu <- nm[yy] %>% as.integer()
    if(identical(vv, uu)) {
      return(op)
    }
  }
  stop(sprintf("No op for %d, %d", u, v))
  NA
}

#' apply_diff for u,v
#'
#' @param far a face array data structure
#' @param u in index of a face
#' @param v an index of an adjoining face
#'
#' @return a translation matrix which will shift the adjoining face
#' so that it aligns with cave u
#' @export
#'
#' @examples
#' far <- example_far
#' apply_difuv(far, 1, 5)
#'
#'
apply_difuv <- function(far, u, v) {
  nmu <- far[nm,,u]
  nmv <- far[nm,,v]
  match <- intersect(nmu, nmv)
  posns <- c(which(nmu %in% match), which(nmv %in% match))
  dif <- far[xy,,u] - far[xy,,v]
  shift = dplyr::case_when(
    identical(posns, c(3L,4L,1L,2L)) ~ c(1, 0),
    identical(posns, c(1L,2L,3L,4L)) ~ c(-1, 0),
    identical(posns, c(1L,4L,2L,3L)) ~ c(0, -1),
    identical(posns, c(2L,3L,1L,4L)) ~ c(0, 1),
    TRUE ~ c(0, 0)
  )
  dif + shift
}

#' is e
#'
#' @param op a operator
#'
#' @return TRUE if OP is the identity of d8, else return FALSE
#'
#'
is_e <- function(op) {
  identical(op, d8[1,])
}

#' adjust face
#'
#' @param far a face array data structure
#' @param rar a ridge array data structure
#' @param u in index of a face
#' @param v an index of an adjoining face
#'
#' @return a copy of the adjoining face which has been rotated, shifted,
#' and aligned so as to fit next to face u in the plane
#' @export
#'
#' @examples
#' far <- example_far
#' rar <- example_rar
#' adjust_face(far, rar, 1, 5)
#'
adjust_face <- function(far, rar, u, v) {
  op <- get_opuv(far, rar, u, v)
  if(!is_e(op)) {
    far[,,v] <- far[,,v] %>% apply_op(op)
  }
  dif <- apply_difuv(far, u, v)
  far[xy,,v] <- far[xy,,v] + dif # fix v
  far
}

