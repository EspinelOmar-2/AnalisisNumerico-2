funcPF<-function(a,b){
  Fx <- function(x) exp(x) -pi*x
  Gx <- function(x) exp(x) / pi
  Hx <- function(x) x
  x<-seq(a,b,0.01)
  plot(x,Fx(x),type="l")
  lines(x, Gx(x),  col = "blue")
  lines(x, Hx(x),  col = "red")
  abline(h=0)
  i<-0
  x<-a
  xi<-0
  ##xi va a ser el x pasado mientras que x es el x actual
  while (b >= x)
  {
    i<-i+1
    if (Gx(x) == x) break
    if ( (Gx(x)>x)&(Gx(xi)<x)  ) break
    if ( (Gx(x)<x)&(Gx(xi)>x)  ) break
    xi<-x
    x<-x+0.01
    c<-abs(x-xi)/abs(x)
    cat("Solucion=",x,"\t Error=",c,"\t Iteracion=",i,"\n")
   
  }
}
funcPF(0,1)