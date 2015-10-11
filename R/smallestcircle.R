getmincir <- function(x, y)
{
  # TODO: add slope NaN condition
  stopifnot(is.numeric(x) && is.numeric(y))
  stopifnot(length(x) == length(y))
  stopifnot(length(x) <=3 && length(y) <=3)
  
  if (length(x) == 1)
    return(c(x,y,0))
  
  d <- dist(cbind(x,y), upper=F)
  
  if (length(x) == 2)
    return(c(mean(x), mean(y), 0.5*as.numeric(d)))
  
  if (isobtuse(x, y) || isline(x, y)){
    d <- melt(as.matrix(d), varnames = c("p1", "p2"))
    md <- which(d$value==max(d$value))
    p <- unique(unlist(d[md,1:2]))
    cx <- 0.5 * (x[p[1]] + x[p[2]])
    cy <- 0.5 * (y[p[1]] + y[p[2]])
    return(c(cx, cy, 0.5*max(d$value)))
  }
  
  slopes <- diff(y) / diff(x)
  cx <- - (0.5 * prod(slopes) * (y[3] - y[1]) + slopes[1] * (x[2] + x[3]) - slopes[2] * (x[1] + x[2]) ) / diff(slopes)
  cy <- - (cx - 0.5*(x[1] + x[2])) / slopes[1] + 0.5 * (y[1] + y[2])
  r <- dist(c(cx, x[1], cy, y[1]))
  return(c(cx, cy, r))
  
}