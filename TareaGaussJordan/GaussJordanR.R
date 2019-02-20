
install.packages("xlsx")
library("xlsx")
A <- matrix(c(2,-5,8,1,-2.5,1,1,-4,6),byrow=T,nrow=3,ncol=3)
b <- matrix(c(-3,5,10),nrow=3,ncol=1)
p <- nrow(A)
(U.pls <- cbind(A,b))

U.pls[1,] <- U.pls[1,]/U.pls[1,1]

i <- 2
cont<-0
while (i < p+1) {
  cont<-cont+1
  j <- i
  while (j < p+1) {
    cont<-cont+1
    U.pls[j, ] <- U.pls[j, ] - U.pls[i-1, ] * U.pls[j, i-1]
    j <- j+1
  }
  while (U.pls[i,i] == 0) {
    cont<-cont+1
    U.pls <- rbind(U.pls[-i,],U.pls[i,])
  }
  U.pls[i,] <- U.pls[i,]/U.pls[i,i]
  i <- i+1
}
cat("iteraciones",cont,"\n")
for (i in p:2){
  for (j in i:2-1) {
    U.pls[j, ] <- U.pls[j, ] - U.pls[i, ] * U.pls[j, i]
  }
}
U.pls