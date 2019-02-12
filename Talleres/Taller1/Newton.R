##Newton
rm(list=ls())
# Halla la raiz de Fx
##funcion de distancia al cuadrado
Fx <- function(x) (((2*cos(x))-2)^2) +((sin(x)-1)^2)
##derivada de la funcion
F1x <- function(x)  -2*(-4*sin(x) + cos(x) + 3*sin(x)*cos(x))

y<-seq(0,2,0.01)
plot(y,Fx(y),type="l")
lines(y, F1x(y),  col = "green")
points(2,1, cex = 2, col = "red")
abline(h=0)
##se grafica el punto al que se debe acercar y las dos funciones
##la derivada en verde, la funcion en negro y el punto en rojo

newton <- function(x) {
  i<-0
  error<-0
  r<-x-Fx(x)/F1x(x)
  while (abs(r-x)>=error){
    i<-i+1
    x<-r
    r<-x-Fx(x)/F1x(x)
    if (Fx(x) == 0) break
    error<-abs(Fx(x)/F1x(x))
    ##se grafican los puntos de la solucion en azul
    points((2*cos(r)),sin(r), cex = .7, col = "blue")
    cat("Iteracion=",i,"\t","Solucion=",r,"\t","Error=",error,"\n")
  }
}
newton(0)