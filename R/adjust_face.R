#' Apply an operation
#'
#' @description
#' Transform the face with the operation, and then align it,
#'
#' @param face a face to transform
#' @param op a transformation operator
#'
#' @return the transformed face
#' @export
#'
#' @examples
#' face <- c(7,0,0,4,0,1,10,1,1,14,1,0) %>%
#'   array(dim = c(3,4), dimnames = list(c('nm', 'x', 'y'), letters[1:4]))
#' apply_op(face, [d8[4,]])
#'
apply_op <- function(face, op) {
  if(is.null(face)) {
    stop("face is null")
  }
  face[xy,] <- face[xy,][ ,op]
  align_face(face)
}

matchuv <- function(u,v) {
  m1 <- which(rar[,1:2] == u, arr.ind = TRUE)
  m2 <- which(rar[,1:2] == v, arr.ind = TRUE)
  rix <- intersect(m1[,1], m2[,1])
  rar[rix,]
}

get_other_edge <- function(edge) {
  rbind(
    oar %>% filter(a == edge[1] & b == edge[2]) %>% select(c,d),
    oar %>% filter(c == edge[1] & d == edge[2]) %>% select(a,b)
  ) %>% as.integer()
}

get_opuv <- function(far, u, v) {
  nu <- far[1,,u]
  vv <- matchuv(u,v)[3:4] %>% as.integer()

  xx <- match(vv, nu)
  yy <- get_other_edge(xx)

  for(i in 1:8) {
    op <- d8[i,]
    nm <- (far[,,v] %>% apply_op(op))[1,]
    uu <- nm[yy] %>% as.integer()
    if(identical(vv, uu)) {
      return(op)
    }
  }
  stop(sprintf("No op for %d, %d", u, v))
  NA
}

apply_difuv <- function(far, u, v) {
  nmu <- far[nm,,u]
  nmv <- far[nm,,v]
  match <- intersect(nmu, nmv)
  posns <- c(which(nmu %in% match), which(nmv %in% match))
  dif <- far[xy,,u] - far[xy,,v]
  shift = case_when(
    identical(posns, c(3L,4L,1L,2L)) ~ c(1, 0),
    identical(posns, c(1L,2L,3L,4L)) ~ c(-1, 0),
    identical(posns, c(1L,4L,2L,3L)) ~ c(0, -1),
    identical(posns, c(2L,3L,1L,4L)) ~ c(0, 1),
    TRUE ~ c(0, 0)
  )
  dif + shift
}

is_e <- function(op) {
  identical(op, d8[1,])
}

adjust_face <- function(far, u, v) {
  op <- get_opuv(far, u, v)
  if(!is_e(op)) {
    far[,,v] <- far[,,v] %>% apply_op(op)
  }
  dif <- apply_difuv(far, u, v)
  far[xy,,v] <- far[xy,,v] + dif # fix v
  far
}

