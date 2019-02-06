funcSecante <- function(x0,x1) {
  Fx <- function(x) exp(x) -pi*x
  ##Fxd1 es la funcion x en la primera derivada
  Fxd1 <- function(x) exp(x) -pi
  x<-seq(x0,x1,0.01)
  plot(Fx(x),type="l")
  abline(h=0)
  x<-(Fx(x1)*x0-Fx(x0)*x1)/(Fx(x1)-Fx(x0))
  c<-1
  i<-0
  while (c > 1.e-8) {
    i<-i+1
    x0<-x1
    x1<-x
    x<-(Fx(x1)*x0-Fx(x0)*x1)/(Fx(x1)-Fx(x0))
    if (Fx(x) == 0) break
    c<-abs(Fx(x)/Fxd1(x))
    cat("Solucion=",x,"\t Error=",c,"\t Iteracion=",i,"\n")
  }
}
funcSecante(0,1)