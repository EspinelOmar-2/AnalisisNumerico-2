# Remueve todos los objetos creados
rm(list = ls())
# Halla la raiz n-esima de a
raizN<-function(a,n)
{
  Fx<-function(x) a-x^n
  i<-0
  while ((i^n < a))
  {
    i <- i + 1
  }
  j <- i - 1
  
  x<-seq(j,i,0.1)
  x<-i
  d<-(j+i)/2
  k<-0
  error<-abs((a/x^n)-1)
  while (error > 1.e-4)
  {
    k<-k+1
    if (Fx(x) == 0) break
    if (Fx(x)*Fx(j) < 0) i <- x else {j <- x}
    d<-x
    x<-(j+i)/2
    error<-abs((a/x^n)-1)
    cat("X=",x,"\tE=",error, " \titeracion: ",k ,"\n" )
  }
  cat("X=",x,"\tE=",error, " \titeracion: ",k ,"\n" )
}
raizN(8,4)