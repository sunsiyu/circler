isline <- function(x, y)
{
  stopifnot(is.numeric(x) && is.numeric(y))
  stopifnot(length(x) == length(y))
  
  return(do.call(identical, as.list(diff(y)/diff(x))))
}

isobtuse <- function(x, y)
{
  stopifnot(is.numeric(x) && is.numeric(y))
  stopifnot(length(x) == length(y))
  stopifnot(length(x) <=3 && length(y) <=3)
  
  d <- dist(cbind(x,y))
 
  return (2*max(d) > sum(d))
}

isright <- function(x, y)
{
  stopifnot(is.numeric(x) && is.numeric(y))
  stopifnot(length(x) == length(y))
  stopifnot(length(x) <=3 && length(y) <=3)
  
  d <- dist(cbind(x,y))
  
  return (2*max(d) == sum(d))
}


isacute <- function(x, y)
{
  stopifnot(is.numeric(x) && is.numeric(y))
  stopifnot(length(x) == length(y))
  stopifnot(length(x) <=3 && length(y) <=3)
  
  d <- dist(cbind(x,y))
  
  return (2*max(d) < sum(d))
}
  
