funcBiseccion<-function(a,b){
  Fx <- function(x) exp(x) -pi*x
  x<-seq(a,b,0.01)
  plot(x,Fx(x),type="l")
  abline(h=0)
  i<-0
  x<-b
  c<-abs(a-b)/2
  while (c > 1.e-8)
  {
    if (Fx(x) == 0) break
    if (Fx(x)*Fx(a) < 0) b <- x 
    else {a <- x}
    i<-i+1
    x<-(a+b)/2
    c<-abs(a-b)/2
    points(x,0, cex = .8, col = "red")
    cat("Solucion=",x,"\t Error=",c,"\t Iteracion=",i,"\n")
  }
}
funcBiseccion(0,1)