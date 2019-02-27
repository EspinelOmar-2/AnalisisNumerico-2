library(pracma)
library(Matrix)
library(psych)
library(rootSolve)
library(matlib)
library(xlsx)
library(BB)
#install.packages("BB")
#install.packages("rootSolve")
#install.packages("psych")
#install.packages("xlsx")
#install.packages("matlib")
##Primero



n<-4
A = matrix(c(-8.1, -7, 6.123, -2, -1, 4,
             -3, -1, 0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)

D1<-eye(n, m = n)
D2<-ones(n, m = n)
D3<-zeros(n, m = n)


funcTransicionSOR<-function(A,n,w){
D<- (A*eye(n, m = n)) #Diagonal de A
L<-A
L[lower.tri(L, diag = FALSE) ]<-0 #triangular inferior
aux<-(D-(w*L))
auxI<-inv(aux)
U<-A
U[upper.tri(U, diag = FALSE) ]<-0#triangular superior
aux2<-((1-w)*D)+(U*w)
Tra<-auxI*aux2
Tra
}
funcTransicionSOR(A,4,1.2)

##Segundo
A2 = matrix(c(-8.1, -7, 6.123, -2, -1, 4,
             -3, -1, 0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)
A2

descomponer<-function(A,n){
  D<- (A*eye(n, m = n)) #Diagonal de A
  L<-A
  L[lower.tri(L, diag = TRUE) ]<-0 #triangular inferior
  L
  U<-A
  U[upper.tri(U, diag = TRUE) ]<-0#triangular superior
  des<-D+L+U
  des
}
descomponer(A2,4)

##Segundo B
A = matrix(c(-8.1, -7, 6.123, -2, -1, 4,
              -3, -1, 0, -1, -5, 0.6,
              -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)
b <- matrix(c(1.45,3,5.12,-4),nrow=4,ncol=1)

itersolve(A, b, tol = 1e-9, method = "Gauss-Seidel")

##Segundo C
A = matrix(c(-8.1, -7, 6.123, -2, -1, 4,
             -3, -1, 0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)
b <- matrix(c(1.45,3,5.12,-4),nrow=4,ncol=1)

itersolve(A, b, x0 = 1:4, nmax = 5, tol = 1e-9, method = "Jacobi")$x


#Tercero
tril1 <- function(M, k = 0) {
  if (k == 0) {
    M[upper.tri(M, diag = FALSE)] <- 0
  } else {
    M[col(M) >= row(M) + k + 1] <- 0
  }
  return(M)
}
A3 = matrix(c(-8.1, -7, 6.123, 
             -3, -1, 0,
             -1, 0.33,  1/2), nrow=3, byrow=TRUE)
tril1(A3,0)


#Tercero A

A3 = matrix(c(-8.1, -7, 6.123, 
              -3, -1, 0,
              -1, 0.33,  1/2), nrow=3, byrow=TRUE)

ValProp <- function(A){
  PolProp <- function(lambda) {
    y = A
    diag(y) = diag(A) - lambda
    return(det(y))
  }
    FunProp <- function(lambda){
    sapply(lambda, PolProp)
  }
  uniroot.all(FunProp, c(-10, 10))
}

ValProp(A3)

eigen(A3)$values



##Tercero B C D
A = matrix(c(4, -1, -1, -1, -1, 4,
             -1, -1, -1, -1, 4, -1,
             -1, -1, -1, 4), nrow=4, byrow=TRUE)
b = c(1, 5, 1.5,-2.33)

A = matrix(c(4, -1, -1, -1, -1, 4,
             -1, -1, -1, -1, 4, -1,
             -1, -1, -1, 4), nrow=4, byrow=TRUE)
A
b = c(1, 5, 1.5,-2.33)
b
itersolve(A, b, tol = 1e-5, method = "Gauss-Seidel")
itersolve(A, b, x0 = 1:4,  tol = 1e-5, method = "Jacobi")

solve(A,b)

#Cuarto
A <- matrix(c(20, 16, -18, -28, -2, 14, 16, -28, -25, 17, -21, 3, -13, 12, -1, 18, -10, 13, -6, 6, -29, 2, -26, -6, 12),byrow=T,nrow=5,ncol=5)
b <- matrix(c(-15, 26, -24, 18, 23),nrow=5,ncol=1)
p <- nrow(A)
(U.pls <- cbind(A,b))

U.pls[1,] <- U.pls[1,]/U.pls[1,1]

i <- 2
cont<-0
while (i < p+1) {
  j <- i
  while (j < p+1) {
    cont<-cont+1
    U.pls[j, ] <- U.pls[j, ] - U.pls[i-1, ] * U.pls[j, i-1]
    j <- j+1
  }
  while (U.pls[i,i] == 0) {
    U.pls <- rbind(U.pls[-i,],U.pls[i,])
  }
  U.pls[i,] <- U.pls[i,]/U.pls[i,i]
  i <- i+1
}

for (i in p:2){
  for (j in i:2-1) {
    cont <-cont+1
    U.pls[j, ] <- U.pls[j, ] - U.pls[i, ] * U.pls[j, i]
  }
}
U.pls
cat("Multiplicaciones",cont,"\n")

#Quinto

jacobi<-function(a,ciclos) {
  n<-nrow(a)
  id <- diag(x = 1, nrow=n, ncol=n )
  Q<-id
  for (k in 1:ciclos) {
    for ( i in 1:(n-1)) {
      for ( j in (i+1):n) {
        control <- 10^(-k)
        if( abs(a[i,j]) > control) {
          print(c(a[i,j],control))
          angulo <- 0.5*atan(2*a[i,j]/(a[i,i] - a[j,j]))
          c<-cos(angulo)
          s<-sin(angulo)
          p<-id
          p[i,i]<-c
          p[j,j]<-c
          p[i,j]<--s
          p[j,i]<- s
          Q <- Q%*%p
          a<-t(p)%*%a%*%p
          a[i,j]<-0
          a[j,i]<-0
          
        }
      }
    }
    cat("estado: ", a, "\n")
  }
  return(list(raices=diag(a),vectores=Q,estado=a))
}

# Aplicacion
A<-rbind(c(2, 0, -1),c(1, 2,-1),c(-1,1,2))
jacobi(A,10)

Jacobi<- function(ciclos){
  A <- matrix(c(2, 0, -1,1, 2,-1,-1,1,3),nrow=3,ncol=3)
  B<- matrix(c(1,2,1), nrow =3, ncol=1)
  Xk<-matrix(c(1,2,3),nrow =3, ncol =1)
  L<- lower.tri(A, diag = FALSE)
  U<- upper.tri(A, diag = FALSE)
  D<- diag(x=1,nrow =3 ,ncol=3, names = TRUE)
  R<-L+U
  
  
  cat("X0: ", Xk, "\n")
  for (k in 1:ciclos)
  {
    P<-(R%*%Xk)
    Xk<-(1/det(A)*(D))%*%(B-P)
    cat("X",k,": ", Xk, "\n")
    k<-k+1
  }
}
Jacobi(10)


#Sexto
Descomponer<-function(nada){
  A <- matrix(c(-8.1, -7.00 , 6.123, -2.0,-1.0 , 4.00, -3.000, -1.0, 0.0, -1.00, -5.000,  0.6, -1.0,  0.33,  6.000,  0.5),nrow=4,ncol=4)
  D<- diag(x=1,nrow =4 ,ncol=4, names = TRUE)
  L<- lower.tri(A, diag = FALSE)
  U<- upper.tri(A, diag = FALSE)
  R<-L+U
  
  R1<- (1/det(R)*t(R))
  Q<-A %*% R1
  
  cat("A = QR:", "\n")
  
  cat(Q %*% R, "\n")
}
Descomponer(0)

#Septimo
funcSecante <- function(x0,x1) {
  Fx <- function(x) sqrt(1-x^2) -x
  ##Fxd1 es la funcion x en la primera derivada
  Fxd1 <- function(x) -(x/(sqrt(1-x^2))) -1
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
funcSecante(-1,1)

trigexp = function(x) {
  n = length(x)# n = longitud de vector x
  F = rep(NA, n) # F es una replica de un vector de tamaño n
  F[1] = 3*x[1]^2 + 2*x[2] - 5 + sin(x[1] - x[2]) * sin(x[1] + x[2])
  tn1 = 2:(n-1)
  F[tn1] = -x[tn1-1] * exp(x[tn1-1] - x[tn1]) + x[tn1] *
    ( 4 + 3*x[tn1]^2) + 2 * x[tn1 + 1] + sin(x[tn1] -
                                               x[tn1 + 1]) * sin(x[tn1] + x[tn1 + 1]) - 8
  F[n] = -x[n-1] * exp(x[n-1] - x[n]) + 4*x[n] - 3
  F
  #se han asignado polinomios a cada dato de F porque ahora F es un vector de polinomios
}
n = 10000
p0 = runif(n) # n initial random starting guesses
sol = BBsolve(par=p0, fn=trigexp)#intenta resolver el vector (ahora matriz) F usando numeros entre 
#0 y 10000 como posibles soluciones al sistema de ecuaciones nolineal fn 
sol$par
