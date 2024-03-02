#' Depth First
#'
#' @param g an igraph graph
#' @description
#' Recurse through the adjacency matrix of the graph, looking for row/column
#' intersections, while keeping track of intersections that have already been seen
#'
#' @return a list of indices of edges that are traversed in depth first order
#' @export
#'
#' @examples
#' graph <- rgraph6::igraph_from_text("KP@?Y_AG?C?G")[[1]]
#' depth_first_gr(graph)
#'
depth_first_gr <- function(g) {
  ajmx <- as_adj(g)
  i <- 1
  len <- nrow(ajmx)

  dfs <- function(from, i, vs, visited) {
    eid <- get.edge.ids(g, c(from, i))
    if(eid > 0) {
      vs <- c(vs, eid)
    }
    visited[[i]] <- TRUE
    for(j in len:1) {
      if(!visited[j] && ajmx[i,j] == 1) {
        vv <- dfs(i, j, vs, visited)
        vs <- vv[[1]]
        visited <- vv[[2]]
      }
    }
    list(vs, visited)
  }
  dfs(1, 1, c(), rep(FALSE, len))[[1]]
}

#' Get edge vertex ids
#' @description
#' First call depth_first_gr, then convert the edge ids to pairs of vertex ids,
#' using a vector of vertex names for guidance
#'
#' @param gr an igraph graph object
#' @param vnames a character vector of vertex names
#'
#' @return a matrix of vertix ids
#' @import igraph
#' @export
#'
#' @examples
#' vnames <- as.character(1:14)
#' graph <- rgraph6::igraph_from_text("KP@?Y_AG?C?G")[[1]]
#' get_evids(graph, vnames)
#'
get_evids <- function(gr, vnames) {
  eids <- gr %>% depth_first_gr()
  evs <- apply(gr %>% as_edgelist(), 1, function(e) {
    i <- which(vnames == e[1])
    j <- which(vnames == e[2])
    c(i, j) %>% sort()
  })
  evt <- evs[ ,eids] %>% t()
  evids <- matrix(0, nrow(evt), ncol(evt))
  seen <- c()
  for(k in 1:nrow(evt)) {
    row <- evt[k,]
    if(row[2] %in% seen) {
      evids[k,] <- c(row[2], row[1])
    } else {
      evids[k,] <- row
      seen <- c(seen, row[2])
    }
  }
  evids
}

