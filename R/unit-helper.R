#' Convert degrees to radians
#' 
#' @param deg A numeric vector, degrees to be converted
#' @return The radians converted from degrees
#' @examples 
#' rad <- deg2rad(180)
#' rads <- deg2rad(seq(0, 360, 90))
#' @export
deg2rad <- function(deg)
{
  if (!is.numeric(deg)) {
    stop("Not numeric: deg!")
  }
  return (deg * pi / 180)
}

#' Convert radians to degrees
#' 
#' @param rad A numeric vector, radians to be converted
#' @return The degrees converted from radians
#' @examples 
#' deg <- rad2deg(pi)
#' degs <- rad2deg(seq(0, 2*pi, pi/2))
#' @export
rad2deg <- function(rad)
{
  if (!is.numeric(rad)) {
    stop("Not numeric: rad!")
  }
  return (rad * 180 / pi)
}