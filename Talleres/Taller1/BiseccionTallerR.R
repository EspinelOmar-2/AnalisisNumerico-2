##Punto 4 Taller 1 Solucion por metodo de Biseccion

rm(list=ls())

polar <- function (theta, r, color=4){
  y <- 0
  x <- 0
  ejex <- 1
  
  for (i in 1:length(r)){
    if(is.nan(r[i])== T){
      r[i] <- 0
    }
  }
  
  angulo <- seq(-max(theta),max(theta),by=theta[2]-theta[1])
  y <- r*sin(theta)
  x <- r*cos(theta)
  plot.new()
  plot.window(xlim = c(-max(r), max(r)), ylim = c(-max(r), max(r)), asp = 1)
  
  aux <- max(r)
  # Dibuja los ejes.
  while (aux > 0){
    fi <- aux*sin(angulo)
    cir <- aux*cos(angulo)
    points(cir,fi,pch="-",col="gray",cex=0.3)
    text(ejex+0.2,-0.2,ejex,col="gray")
    ejex <- ejex + 1
    aux <- aux - 1
  }
  
  abline(v=((max(cir)+min(cir))/2),col="gray")
  abline(h=((max(cir)+min(cir))/2),col="gray")
  segments(-max(r)+0.5,-max(r)+0.5,max(r)-0.5,max(r)-0.5,col="gray")
  segments(-max(r)+0.5,max(r)-0.5,max(r)-0.5,-max(r)+0.5,col="gray")
  
  points(x,y,pch=20,col=color,cex=1)
}
dim <- seq(-pi, pi, by=pi/300) 
r= cos(3*dim)+exp(dim)
polar(dim,r,"blue")

funcBiseccion<-function(a,b){
  Fx <- function(x) cos(3*x)+exp(x)
  i<-0
  x<-b
  c<-abs(a-b)/2
  while (c > 1.e-7)
  {
    if (Fx(x) == 0) break
    if (Fx(x)*Fx(a) < 0) b <- x 
    else {a <- x}
    i<-i+1
    x<-(a+b)/2
    c<-abs(a-b)/2
    yp <- Fx(x)*sin(x)
    xp <- Fx(x)*cos(x)
    points(xp,yp, cex = .8, col = "red")
    cat("Solucion=",x,"\t Error=",c,"\t Iteracion=",i,"\n")
  }
}
funcBiseccion(-1,0)