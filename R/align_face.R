#' xy
#'
#' @return the indices of the rows in a face_array data structure that
#' contain the x and y coordinates
#'
#' @examples
#' far <- example_far
#' xy <- c(2,3)
#' far[xy,,1]
#'
xy <- c(2,3) # vertex coordinates

#' nm
#'
#' @return the index of the row in a face_array data structure that
#' contain the names of the corners of the faces
#'
#' @examples
#' far <- example_far
#' nm <- 1
#' far[nm,,1]
#'
nm <- c(1) # name

#' Get angle
#'
#' @description
#' Given a center point and a periferal point, determine the angle
#' of the corresponding plane vector with respect to the line pointing
#' towards the 'South Pole', going clockwise from 6:00
#'
#'
#' @param cent the center point
#' @param perif the periferal point
#'
#' @return a number between 0 and 360
#' @export
#'
#' @examples
#' get_angle(c(0,0), c(1,1)) # expect 225
#'
get_angle <- function(cent, perif){
  deltaX = perif[1] - cent[1]
  deltaY = perif[2] - cent[2]
  ang <- atan2(deltaX, deltaY) #clockwise
  deg <- ang * (180/pi)
  if(deg < 0) deg <- 360 + deg
  (deg + 180) %% 360 #start at 6:00
}

#' get_angles
#'
#' @param mx a 2x4 matrix of vertex coordinates
#'
#' @return a vector of angles for the vertices in mx
#' @export
#'
#' @examples
#' face <- array(c(7,0,0,4,0,1,10,1,1,14,1,0), dim = c(3,4),
#' dimnames = list(c('nm', 'x', 'y'), letters[1:4]))
#' mx <- face[c('x','y'),]
#' get_angles(mx) # expect 45 135 225 315
#'
get_angles <- function(mx) {
  rm <- mx %>% data.frame() %>% rowMeans()
  angles <- apply(mx, 2, function(col) {get_angle(rm, col)})
  angles
}

#' Check angles
#'
#' @param angles a vector of 4 angles, in degrees from 0 to 360
#'
#' @return TRUE if angles increase clockwise, else FALSE
#' @export
#'
#' @examples
#' check_angles(c(45, 135, 225, 315)) # expect TRUE
#' check_angles(c(145, 35, 225, 315)) # expect FALSE
#'
check_angles <- function(angles) {
  angles[1] < angles[2] & angles[2] < angles[3] & angles[3] < angles[4]
}

#' Align face
#'
#' @param face a face of a polyhedron, as a 'face_array' structure
#'
#' @return the face with its columns rearranged so as to pass the check_angles check
#' @export
#'
#' @examples
#' face <- array(c(14,0,0,7,0,1,6,1,1,12,1,0), dim = c(3,4),
#' dimnames = list(c('nm', 'x', 'y'), letters[1:4]))
#' align_face(face)
align_face <- function(face) {
  f <- face
  rows <- 1:nrow(face)
  angles <- get_angles(face[xy,])
  if(! check_angles(angles)) {
    for(j in 1:8) {
      if(check_angles(get_angles(face[xy,d8[j,]]))) {
        f[rows,] <- face[rows,d8[j,]] # rearrange columns
        break
      }
    }
  }
  f
}
