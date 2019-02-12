## 1.Calcular en el menor numero de operaciones P(X)
##P(x)=(2*x^4)-(3*x^2)+(3*x)-4 , X0=-2
P1<-(-4)
x<-(-2)
P1<- P1+(3*(x))##2
x<-x*(-2)#1
P1<- P1-(3*(x))##2
x<-x*(8)#1
##8=(-2^2)*2
P1<- P1+((x))#1
cat("Primer P(x)=",P1,"\n")

##P(x)=(7*x^5)+(6*x^4)-(6*x^3)+(3*x)-4 , X0=3

P2<-(-4)
x<-(9)
P2<- P2+((x))##2
x<-x*(3)#1
P2<- P2-(6*(x))##2
x<-x*(3)#1
P2<- P2+(6*(x))#1
x<-x*(21)#1
P2<- P2+((x))#1
cat("Segundo P(x)=",P2,"\n")

##P(x)=(-5*x^6)+(3*x^4)+(2*x^2)-(4*x) , X0=-1


P3<-(-4*(-1))
P3<-P3+(2*1)
P3<-P3+(3*1)
P3<-P3-(5*1)
cat("Tercero P(x)=",P3,"\n")
##2. Demuestre que el numero minimo de multiplicaciones es n siendo en el grado del polinomio
#Un polinomio de grado n tiene la siguiente forma:
# P(x) a0+a1*x^1+a2*x^2+...+an-1*x^(n-1)+an*x^n
# La forma mas simple (la que tiene menos terminos, y por consiguiente menos multiplicaciones)
# de un polinomio de grado n es P(x) = an*x^n lo cual se puede expresar tambien como:
# P(x)= an* (x*x*...*x)
# x^n puede ser expresado como la multiplicacion de n x iguales, 
# la cantidad de multiplicaciones entre x necesarias para formar x^n es n-1
# para obtener P(x) aun es necesario multiplicar x^n por un numero an, lo cual añade
# una multiplicacion mas, asi que para tener el polinomio mas simple de grado n: P(x)=an*x^n 
# es necesario n-1 +1 multiplicaciones, por lo que el numero minimo de multiplicaciones 
# para obtener un polinomio de grado n seria n
