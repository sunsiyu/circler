deg2rad <- function(deg)
{
  if (!is.numeric(deg)) {
    stop("Not numeric: deg!")
  }
  return (deg * pi / 180)
}


rad2deg <- function(rad)
{
  if (!is.numeric(rad)) {
    stop("Not numeric: rad!")
  }
  return (rad * 180 / pi)
}

getarea <- function(radius)
{
  if (!is.numeric(radius)) {
    stop("Not numeric: radius!")
  }
  return (pi * radius * radius)
}

getdiameter <- function(area)
{
  if (!is.numeric(area)) {
    stop("Not numeric: area!")
  }
  return (2 * sqrt(area / pi))
}

getradius <- function(area)
{
  if (!is.numeric(area)) {
    stop("Not numeric: area!")
  }
  return(sqrt(area / pi))
}





