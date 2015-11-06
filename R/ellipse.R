simpleellipse <- function(x0, y0, a, b)
{
  theta <- seq(0, 2 * pi, length=100)
  x <- x0 + a * cos(theta)
  y <- y0 + b * sin(theta)
  plot(x, y, type = "l")
}
