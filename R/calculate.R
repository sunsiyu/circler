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



