#' Plot circle with different methods
#' 
#' @param x,y coordinates of circle center
#' @param r   radius of circle
#' @param npoints number of points
#' @param method  plotting method
#' @param asp     aspect default is 1
#' @param xlab,ylab x and y labels
#' @param col color of circle
#' @return invisible NULL
#' @examples 
#' plotcircle(0,0,1)
#' @export
plotcircle <- function(x=0, y=0, r=1, npoints=100, method="plot", 
                       asp=1, xlab="x", ylab="y", col="black", ...)
{
  if(!is.numeric(x) || !is.numeric(y) || !is.numeric(r))
    stop("Not numeric: x or y or radius!")
  
  pts <- seq(0, 2*pi, length.out = npoints)
  rx <- x + r * sin(pts)
  ry <- y + r * cos(pts)
  
  switch(method,
         
         plot = {plot(rx, ry, type="l", 
                      asp=asp, xlab=xlab, ylab=xlab, col=col...)},
         
         polygon = {plot(rx, ry, asp=asp, xlab=xlab, ylab=ylab, type="n")
                    polygon(rx, ry, border=col, ...)}
         )
  return(invisible(NULL))
}
  
  
  
