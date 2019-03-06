library(Matrix)

library(PolynomF)

##Puntos 

x=c(6, 8, 10, 12, 14, 16, 18,20)

  y=c(7, 9, 12, 18, 21, 19, 15, 10)    
  DatosX = x[1:8]; DatosY = y[1:8]
  Ajuste_Polinomio = poly.calc(DatosX,DatosY)
  Ajuste_Polinomio
  plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Temp")
  points(DatosX,DatosY, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
  curve(Ajuste_Polinomio,add=T,from =6,to =20)
 
   Fx <- function(x) -801 + 461.0976*x - 106.5194*x^2 + 12.84479*x^3 - 0.8676215*x^4 + 0.03268229*x^5 - 0.0006293403*x^6 + 4.650298e-06*x^7 
  Fx(7)
  Fx(9)
  i<-6
  while (i <21 ) {
    e<-abs(Fx(i)-Fx(i-1))/abs(Fx(i))
    points(i,Fx(i), pch=19, cex=.5, col = "blue", asp=1,xlab="X", ylab="Y", main="Mano derecha")
    cat("En=",i,"\t La temp es=",Fx(i),"\tCon error=",e,"\n")
    i<-i+1
    }