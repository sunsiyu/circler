% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/calculate.R
\name{getchord}
\alias{getchord}
\title{Calculate length of chord of a circle}
\usage{
getchord(rad, radius)
}
\arguments{
\item{rad}{degree in radian}

\item{radius}{radius of the circle}
}
\value{
Length of chord of a circle.
}
\description{
Calculate length of chord of a circle
}
\examples{
chord <- getchord(pi, 1)
chords <- getchord(c(0, pi, 2*pi), 1:3)
}

