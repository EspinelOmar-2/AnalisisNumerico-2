
#install.packages("xlsx")
#library("xlsx")
#setwd("C:/Users/Soporte/Documents/Analisis_Numerico/Sistemas_ecuaciones")

A <- matrix(c(5,1,-4,6),byrow=T,nrow=2,ncol=2)
b <- matrix(c(-3,5),nrow=2,ncol=1)
p <- nrow(A)
(Matriz <- cbind(A,b))

Matriz[1,] <- Matriz[1,]/Matriz[1,1]

i <- 2
cont<-0
while (i < p+1) {
  cont<-cont+1
  j <- i
  while (j < p+1) {
    cont<-cont+1
    Matriz[j, ] <- Matriz[j, ] - Matriz[i-1, ] * Matriz[j, i-1]
    j <- j+1
  }
  while (Matriz[i,i] == 0) {
    cont<-cont+1
    Matriz <- rbind(Matriz[-i,],Matriz[i,])
  }
  Matriz[i,] <- Matriz[i,]/Matriz[i,i]
  i <- i+1
}

for (i in p:2){
  for (j in i:2-1) {
    cont<-cont+1
    Matriz[j, ] <- Matriz[j, ] - Matriz[i, ] * Matriz[j, i]
  }
}
cat("iteraciones",cont,"\n")
write.xlsx(Matriz,file="Tablon.xlsx")
Matriz
