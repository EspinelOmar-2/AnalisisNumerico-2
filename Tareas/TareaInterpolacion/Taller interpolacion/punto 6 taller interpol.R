library(pracma)
library(PolynomF)
library(Matrix)

rm(list=ls())

fx<-function(x) exp(x)
gx<-function(x) 1/x
p<-taylor(fx, 0, 4)
p
tallerParteA<-function()
{
   
   x <- seq(-1.0, 1.0, length=100)
   yf <- fx(x)
   yp <- polyval(p, x)
   plot(x, yf, type = "l", col = "gray", lwd = 3)
   
   lines(x, yf, col = "blue")
   lines(x, yp, col = "red")
}
tallerParteA()

tallerParteB<-function()
{
  
  x <- seq(-1.0, 1.0, length=100)
  yf <- gx(x)
  yp <- polyval(p, x)
  plot(x, yf, type = "l", col = "gray", lwd = 3)
  
  lines(x, yf, col = "blue")
  lines(x, yp, col = "red")
}
tallerParteB()