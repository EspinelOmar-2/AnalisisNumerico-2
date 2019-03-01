##Omar Espinel Santamaria
#Documento CC: 1019130596
#Parcial 1 Analisis Numerico
#Punto 2 A

rm(list=ls())



funcXn <- function(xa,xb) {
  
  # Fx <- function(x) tan(pi*x)
  Fx <- function(x) tan(pi*x) - sin(pi*x) #se igualan las dos funciones y se despejan para cero
  x<-seq(xa,xb,0.01)
  plot(Fx(x),type="l")
  abline(h=0)
  c<-1
  i<-0
  # se deciden estos valores iniciales de manera arbitraria pero aun asi cercanos al punto que desea el usuario
  x<-xa+.002
  x1<-xa+.001
  x2<-xa
  while ( (c > 1.e-9) || (x<=xb) ) {
    i<-i+1
    x2<-x1
    x1<-x
    
    if((Fx(x1) - Fx(x2))==0){
      cat ("Error, Indeterminacion por division entre cero\n")
      break
    }
    
    x<-( x1 - ( ( Fx(x1) * (x1-x2) )/(Fx(x1) -Fx(x2)) ) ) #Se aplica la funcion para encontrar el valor de x
    
    if(  (x-(floor(x))<0.5) ||  (x-(floor(x))>(3/2))  ){ 
      #si el valor de x hace que la funcion tan(pi*x) salga de sus limites se entra a este if y se recalcula x para evitar que la funcion tienda a inf 
      i<-i+1
      x2<-x1
      x1<-x
      x<-( x1 - ( ( Fx(x1) * (x1-x2) )/(Fx(x1) -Fx(x2)) ) )
    }
    if((Fx(x)==0)  || (abs(Fx(x))<1.e-9) ){
      #Si la raiz da igual a cero o muy cercana (1e-9) a cero
      cat ("Raiz encontrada en",x,"\n")
      break
    }
    c<-abs(Fx(x)-Fx(x1))/abs(Fx(x))
    #se calcula el error
    points(x,0, cex = .8, col = "red")
    cat("Solucion=",x,"Funcion=",Fx(x),"\t Error=",c,"\t Iteracion=",i,"\n")
  }
}
funcXn(0.5,4)

# Punto 2 B

#Se usa el metodo de biseccion para calcular la solucion y se mira la cantidad de operaciones, se agrega un if para descartar indeterminaciones en la funcion Tan(pi*x)

funcBiseccion<-function(a,b){
  Fx <- function(x)  tan(pi*x) - sin(pi*x)
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
    if(  (x-(floor(x))<0.5) ||  (x-(floor(x))>(3/2))  ){
      i<-i+1
      x<-(a+b)/2
    }
    c<-abs(a-b)/2
    points(x,0, cex = .8, col = "red")
    cat("Solucion=",x,"\t Error=",c,"\t Iteracion=",i,"\n")
  }
}
funcBiseccion(0.5,4)



