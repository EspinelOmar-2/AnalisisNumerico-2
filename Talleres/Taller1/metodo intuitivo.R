# Remueve todos los objetos creados
rm(list = ls())

Fx <- function(x) exp(x) - pi * x

intuitivo <- function(a, b)
{
  x <- a
  d <- (b - a) / 10
  error <- abs(Fx(x))
  avanza <- TRUE
  while (error > 1.e-4)
  {
    cambio <- FALSE
    
    r <- Fx(x)
    if (avanza)
    {
      x <- x + d
      r1 <- Fx(x)
    }
    else
    {
      x <- x - d
      r1 <- Fx(x)
    }
    if ((r > 0) && (r1 < 0))
    {
      cambio <- TRUE
    }
    if ((r < 0) && (r1 > 0))
    {
      cambio <- TRUE
    }
    if(cambio)
    {
      d <- d / 10
      avanza <- !avanza
    }
    error <- abs(d/x+d)
    cat("x= ", x, "r= ", r," error= ", error, "\n" )
    
  }
}
intuitivo(0,1) 