#' Calculate area from different variables
#' 
#' @param variable A character with name of variable supplied
#' @param value    A numeric vector corresponds to the variable name
#' @return Circular area calculated from supplied variable
#' @examples 
#' area <- getarea("radius", 1)
#' areas <- getarea("diameter", 1:10)
#' @export
getarea <- function(variable=c("radius", "diameter", "circumference")[1], value)
{
  if (!is.character(variable))
    variable <- as.character(variable)
  
  if (!is.numeric(value))
    stop("Not numeric: value!")
  
  switch(variable, 
         radius = return(pi * value * value),
         diameter = return(0.25 * pi * value * value),
         circumference = return(0.25 * value * value / pi),
         area = return(value)
  )
}


#' Calculate diameter from different variables
#' 
#' @param variable A character with name of variable supplied
#' @param value    A numeric vector corresponds to the variable name
#' @return Diameter calculated from supplied variable
#' @examples 
#' diameter <- getdiameter("area", pi)
#' diameters <- getdiameter("radius", 1:10)
#' @export
getdiameter <- function(variable=c("radius", "diameter", "circumference")[1], value)
{
  if (!is.character(variable))
    variable <- as.character(variable)
  
  if (!is.numeric(value))
    stop("Not numeric: value!")
  
  switch(variable, 
         radius = return(2 * value),
         diameter = return(value),
         circumference = return(value / pi),
         area = return(2 * sqrt(value / pi))
  )
}


#' Calculate radius from different variables
#' 
#' @param variable A character with name of variable supplied
#' @param value    A numeric vector corresponds to the variable name
#' @return Radius calculated from supplied variable
#' @examples 
#' radius <- getradius("area", pi)
#' radii <- getradius("circumference", 1:10)
#' @export
getradius <- function(variable=c("radius", "diameter", "circumference")[1], value)
{
  if (!is.character(variable))
    variable <- as.character(variable)
  
  if (!is.numeric(value))
    stop("Not numeric: value!")
  
  switch(variable, 
         radius = return(value),
         diameter = return(0.5 * value),
         circumference = return(0.5 * value / pi),
         area = return(sqrt(value / pi))
  )
}


#' Calculate circumference from different variables
#' 
#' @param variable A character with name of variable supplied
#' @param value    A numeric vector corresponds to the variable name
#' @return Circumference calculated from supplied variable
#' @examples 
#' circum <- getcircumference("area", pi)
#' circums <- getcircumference("radius", 1:10)
#' @export
getcircumference <- function(variable=c("radius", "diameter", "area")[1], value)
{
  if (!is.character(variable))
    variable <- as.character(variable)
  
  if (!is.numeric(value))
    stop("Not numeric: value!")
  
  switch(name, 
         radius = return(2 * pi * value),
         diameter = return(pi * value),
         area = return(2 * sqrt(pi * value)), 
         circumference = return(value)
  )
}

#' Calculate length of arc of a circle
#' 
#' @param rad    degree in radian
#' @param radius radius of the circle
#' @return Length of arc of a circle.
#' @examples 
#' arc <- getarc(pi, 1)
#' arcs <- getarc(c(0, pi, 2*pi), 1:3)
#' @export
getarc <- function(rad, radius)
{
  if (!is.numeric(rad) || !is.numeric(radius))
    stop("Not numeric: rad or radius!")

  return(rad * radius)
}

#' Calculate length of chord of a circle
#' 
#' @param rad    degree in radian
#' @param radius radius of the circle
#' @return Length of chord of a circle.
#' @examples 
#' chord <- getchord(pi, 1)
#' chords <- getchord(c(0, pi, 2*pi), 1:3)
#' @export
getchord <- function(rad, radius)
{
  if (!is.numeric(rad) || !is.numeric(radius))
    stop("Not numeric: rad or radius!")
  
  return(2 * radius * abs(sin(0.5 * rad)))  
  
}

#' Calculate area of sector of a circle
#' 
#' @param rad    degree in radian
#' @param radius radius of the circle
#' @return Area of sector of a circle
#' @examples 
#' sct <- getsector(pi, 1)
#' scts <- getsector(c(0, pi, 2*pi), 1:3)
#' @export
getsector <- function(rad, radius)
{
  if (!is.numeric(rad) || !is.numeric(radius))
    stop("Not numeric: rad or radius!")
  
  return(0.5 * rad * radius * radius)
  
}

#' Calculate area of segment of a circle
#' 
#' @param rad    degree in radian
#' @param radius radius of the circle
#' @return Area of segment of a circle
#' @examples 
#' seg <- getsegment(pi, 1)
#' segs <- getsegment(c(0, pi, 2*pi), 1:3)
#' @export
getsegment <- function(rad, radius)
{
  if (!is.numeric(rad) || !is.numeric(radius))
    stop("Not numeric: rad or radius!")
  
  asector <- getsector(rad, radius)
  atriangle <- radius * radius * abs(cos(0.5*rad) * sin(0.5*rad))

  return(asector - atriangle)
  
}
  
  
  
  
  
  