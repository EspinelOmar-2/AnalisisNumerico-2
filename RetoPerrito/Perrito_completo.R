library(pracma)
library(Matrix)
library(rSymPy)
rm(list=ls())

#codigo para la eficiencia se toma el tiempo que tarda el codigo desde este punto.
start.time <- Sys.time() 
#Funcion de interpolacion recibe un arreglo y el valor inicial/final de este y devuelve la interpolacion y los puntos dados por esta
InterpolBarLagr<-function(Ini, Fin){
  X <- seq(x[Ini], x[Fin], length=90)
  Interpol <- barylag(x[Ini:Fin], y[Ini:Fin], X)
  lines(X, Interpol, col="black",from =0,to =35)
  indiceJ(X,x, Ini,Fin)
}


#Funcion Jaccard recibe los vectores del mismo tamaño con los puntos en los mismos valores de X y de 
#acuerdo a ello loso divide y redondea si el aprecido o al division es 1 lo toma como un acierto si no como falla y muestra al final

IndiceJaccard(Yr,y ){
  i=1
  a=0
  f=0
  while (i<length(y)) {
    x=(Yr[i])/(y[i])
    if(round(x,digits=1)=1) a=a+1
    else f=f+1
    i=i+1
  }
  cat(" Aciertos=\t",a, "Fallos=",f,"\n")
}

#puntos del perro organizados por la parte de su cuerpo

y=c(3,  3.7 ,3.9 ,
    #cola 1-3
    7.12 ,4.45 ,
    #cuerpo 4-5
    7   ,5.6  ,
    #Cabeza pt1 6-7
    5.95 ,4.6,
    # 5.82,4.6,
    #cabeza pt2 8-9
    
    3, 3,
    #Cola 10-11
    3.1,3.5, 3.2,
    #pata 1.1
    2, 2.4, 3, 3.2,
    #pata1.2
    2, 1.6 ,1.9, 1.6 ,1.9, 1.7, 2,
    #pata2
    2 , 1.6 ,
    #cuerpo
    2 , 
    #oreja
    1.9,1.5,1.8,
    #pata
    2, 3, 4.6
    #cabeza 
)


x=c(1,  2   ,5   ,
    #cola 1-3
    10   ,17.6 ,
    #cuerpo 4-5
    20 ,24.5 ,
    #cabeza pt1 6-7
    #Descomentar y comentar los otros puntos para probar como deforma el perro
    #  24.7  ,28,
    26.5  ,28,
    #cabeza pt2 8-9
    
    1, 7.5,
    #Cola 10-11
    8 ,8.5,  9 ,
    #pata 12-14
    8.4, 8.5, 8.9 , 9,
    #pata1.2 15-18
    8.4, 9.3 , 11, 12.3 , 13.5, 14.1, 15,
    #pata2 19-25
    18 ,18.6,
    #cuerpo 26-27
    19.2, 
    #oreja 28
    20 ,24.4,25,
    #pata 29-31
    27, 27.9,28
    #cabeza 32.34
)

#Lineas para probar el cambio de origen, descomentar y correr, se mueve el perro 10 unidades a la derecha y 10 unidades hacia arriba
#x<- x+10
#y<- y+10


#A continuacion se grafica  


length(x) 

length(y) 

plot(x,y, pch=19, cex=0.5, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito ",from =0,to =35)


#se interpola en cada uno de los puntos y las partes mencionadas y se grafica cada parte, se reaiza la interpolacion para la parte de arriba y la de abajo


InterpolBarLagr(1,3)
InterpolBarLagr(3,5)
InterpolBarLagr(5,7)
InterpolBarLagr(7,9)
#ARRIBA

InterpolBarLagr(10,11)
InterpolBarLagr(11,14)
InterpolBarLagr(15,18)
InterpolBarLagr(19,25)
InterpolBarLagr(25,26)
InterpolBarLagr(26,28)
InterpolBarLagr(28,31)
InterpolBarLagr(31,33)
InterpolBarLagr(33,34)
#ABAJO

##perro ejemplo


x=c(1,  2,  5,               10,    17.6,20,     24.5,     26.5,      28)
y=c(3,3.7,3.9,             7.12,    4.45, 7,      5.6,     5.95,     4.6)

x=c(1,  2,  5,  6,7.5, 8.1,  10, 13,17.6,20,23.5,24.5,  25,26.5,27.5, 28 ,29 ,30)
y=c(3,3.7,3.9,4.5,5.7,6.69,7.12,6.7,4.45, 7, 6.1, 5.6,5.87,5.15, 4.1, 4.3,4.1, 3)  



length(x)
length(y)

points(x,y,pch=18,type = "p",add=T, col= "blue",from =0,to =35)


x=c(1,  2,  5,               10,    17.6,20,     24.5,     26.5,      28)
y=c(3,3.7,3.9,             7.12,    4.45, 7,      5.6,     5.95,     4.6)

##algoritmo tomado de: https://www.r-bloggers.com/lagrangian-polynomial-interpolation-with-r/
lagrange.poly <- function(x, y) 
{
  
  l <- list() # List to store Lagrangian polynomials L_{1,2,3,4}
  k <- 1
  
  for (i in x) {
    # Set the numerator and denominator of the Lagrangian polynomials to 1 and build them up
    num <- 1
    denom <- 1
    
    # Remove the current x value from the iterated list
    p <- x[! x %in% i]
    
    # For the remaining points, construct the Lagrangian polynomial by successively 
    # appending each x value
    for (j in p) {
      num <- paste(num, "*", "(", 'x', " - ", as.character(j), ")", sep = "", collapse = "")
      denom <- paste(denom, "*", "(", as.character(i)," - ", as.character(j), ")", sep = "", collapse = "")
    }
    
    # Set each Lagrangian polynomial in rSymPy to simplify later.
    l[k] <- paste("(", num, ")", "/", "(", denom, ")", sep = "", collapse = "")
    k <- k + 1
  }
  
  # Similar to before, we construct the final Lagrangian polynomial by successively building 
  # up the equation by iterating through the polynomials L_{1,2,3,4} and the y values 
  # corresponding to the x values.
  eq <- 0
  
  for (i in 1:length(y)) {
    eq <- paste(eq, '+', as.character(y[i]), "*", l[[i]], sep = "", collapse = "")
  }
  
  # Define x variable for rSymPy to simplify
  x <- Var('x')
  
  # Simplify the result with rSymPy and return the polynomial
  return(sympy(paste("simplify(", eq, ")")))
}

lagr<- lagrange.poly(x[1:3],y[1:3])
lagr
lagr<- function(x) 1.98333333333333 + 1.175*x - 0.158333333333333*x^2
curve(lagr,add=T,col="green",from =0,to =35)

lagr1<- lagrange.poly(x[3:5],y[3:5])
lagr1
lagr1<- function(x) -3.26966583124478 + 1.82889974937343*x - 0.0789933166248956*x^2
curve(lagr1,add=T,col="green",from =0,to =35)

lagr2<- lagrange.poly(x[5:7],y[5:7])
lagr2
lagr2<- function(x) -84.3240740740741 + 8.54768518518519*x - 0.199074074074074*x^2
curve(lagr2,add=T,col="green",from =0,to =35)

lagr3<- lagrange.poly(x[7:9],y[7:9])
lagr3
lagr3<- function(x) -198.1 + 15.8392857142857*x - 0.307142857142857*x^2

x= c(6,7.5, 8.1, 13,23.5,  25,27.5,29 ,30)
y= c(5,5.7,6.69,6.7, 6.1,5.87, 4.1,4.1, 3)
ye=c(4.859973, 6.003708,6.361671, 6.60787,5.917857, 5.203571)
cat(" x = 6: ", lagr1(6)," x = 7.5: ",lagr1(7.5)," x = 8.1: ",lagr1(8.1)," x = 13: ", lagr1(13))
cat(" x = 23.5: ",lagr2(23.5))
cat(" x = 25: ",lagr3(25)," x = 27.5: ",lagr3(27.5))
e= c(0.0280054,0.05328210526,0.049077577848,0.08596681034,0.01375074627, 0.0298595082,0.1135313458,0.46341446341)

M<-matrix(c(6,7.5, 8.1, 13,23.5,  25,27.5,5,5.7,6.69,6.7, 6.1,5.87, 4.1,4.859973, 6.003708,6.361671, 6.60787,5.917857, 5.203571,0.0280054,0.05,0.05,0.09,0.01, 0.03,0.11,0.46),ncol=4,nrow=7)
colnames(M) <- c("x","y","ye","error")
M <- as.table(M)
M

##hasta aca llegan las graficas del perrito------------------------------------------------------

x=c(25,26.5,27.5, 28 ,29 ,30)
y=c(5.87,5.15, 4.1, 4.3,4.1, 3)
#Segunda parte del codigo para eficiencia
#Se toma y se muestra el tiempo que tarda el codigo hasta aqui
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

y=c(3,3.7,3.9,4.5,5.7,6.69,7.12,6.7,4.45,7,6.1,5.6,5.87,5.15,4.1,4.3,4.1,3)                                                                                                       
x=c(1,2,5,6,7.5,8.1,10,13,17.6,20,23.5,24.5,25,26.5,27.5,28,29,30)     

longitud <- length(x)
longitud
aciertos <- 0
for (i in x) 
{
  cat(x[i])
  if((lagr(x[i])<= y[i]+0.5)&&(lagr(x[i])>= y[i]-0.5))
  {
    aciertos <- aciertos + 1
  }
}
for (i in x) 
{
  if((lagr1(x[i])<= y[i]+0.5)&&(lagr1(x[i])>= y[i]-0.5))
  {
    aciertos <- aciertos + 1
  }
}
for (i in x) 
{
  if((lagr2(x[i])<= y[i]+0.5)&&(lagr2(x[i])>= y[i]-0.5))
  {
    aciertos <- aciertos + 1
  }
}
for (i in x) 
{
  if((lagr3(x[i])<= y[i]+0.5)&&(lagr3(x[i])>= y[i]-0.5))
  {
    aciertos <- aciertos + 1
  }
}

cat("indice de Jaccard: ", aciertos/longitud, "aciertos:", aciertos  )