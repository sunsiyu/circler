#' Determine if multiple points are on one line
#' 
#' @param x  x coordinates of points
#' @param y  y coordinates of points
#' @return logical. TRUE indicates on one line
#' @examples 
#' isline(c(0,0), c(1,1)) # TRUE
#' isline(1:3, rnorm(3))  # probably FALSE
#' @export
isline <- function(x, y)
{
  stopifnot(is.numeric(x) && is.numeric(y))
  stopifnot(length(x) == length(y))
  
  return(do.call(identical, as.list(diff(y)/diff(x))))
}

#' Determine if a triangle is obtuse
#' 
#' @param x  x coordinates of vertices of a triangle
#' @param y  y coordinates of vertices of a triangle
#' @return logical. TRUE indicates the triangle is obtuse
#' @examples 
#' isobtuse(1:3, c(2, 8, 9)) # TRUE
#' isobtuse(c(0,0,3), c(0,4,0))  # FALSE
#' @export
isobtuse <- function(x, y)
{
  stopifnot(is.numeric(x) && is.numeric(y))
  stopifnot(length(x) == length(y))
  stopifnot(length(x) <=3 && length(y) <=3)
  
  d <- dist(cbind(x,y))
 
  return (2*max(d*d) > sum(d*d))
}

#' Determine if a triangle is right
#' 
#' @param x  x coordinates of vertices of a triangle
#' @param y  y coordinates of vertices of a triangle
#' @return logical. TRUE indicates the triangle is right
#' @examples 
#' isright(1:3, c(2, 8, 9)) # FALSE
#' isright(c(0,0,3), c(0,4,0))  # TRUE
#' @export
isright <- function(x, y)
{
  stopifnot(is.numeric(x) && is.numeric(y))
  stopifnot(length(x) == length(y))
  stopifnot(length(x) <=3 && length(y) <=3)
  
  d <- dist(cbind(x,y))
  
  return (2*max(d*d) == sum(d*d))
}


#' Determine if a triangle is acute
#' 
#' @param x  x coordinates of vertices of a triangle
#' @param y  y coordinates of vertices of a triangle
#' @return logical. TRUE indicates the triangle is acute
#' @examples 
#' isacute(1:3, c(2, 8, 9)) # FALSE
#' isacute(c(0,0,3), c(0,4,0))  # FALSE
#' @export
isacute <- function(x, y)
{
  stopifnot(is.numeric(x) && is.numeric(y))
  stopifnot(length(x) == length(y))
  stopifnot(length(x) <=3 && length(y) <=3)
  
  d <- dist(cbind(x,y))
  
  return (2*max(d*d) < sum(d*d))
}
  
