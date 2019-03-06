##Analisis numerico 

install.packages("Matrix")#instalar paquete
library(Matrix)
install.packages("PolynomF")#instalar paquete
library(PolynomF)

##Puntos 

x=c(14.6, 14.7, 14.6, 14.8, 15.2, 15.6, 15.7, 17.0, 17.6, 17.5, 17.3, 16.8, 15.4, 14.8, 14.4, 14.5, 15.0, 15.1, 15.0, 14.9, 14.6, 14.3, 14.0, 13.9, 13.8, 13.5, 13.1, 13.0, 13.3, 13.2, 13.1, 12.9, 12.4, 11.9, 11.7, 11.6, 11.3, 10.9, 10.7, 10.6, 10.6, 10.1, 9.7, 9.4, 9.3, 9.6, 9.9, 10.1, 10.2, 10.3, 9.10, 8.6, 7.5, 7.0, 6.7, 6.6, 7.70, 8.00, 8.10, 8.40,              9.00, 9.30, 10, 10.2, 10.3, 10.0, 9.50)                                                                                                       
y=c(14.7, 14.0, 13.4, 12.3, 11.0, 10.5, 10.2, 8.20, 7.10, 6.70, 6.60, 6.80, 8.30, 8.80, 9.30, 8.80, 6.30, 5.50, 5.00, 4.70, 4.60, 4.50, 4.90, 5.40, 5.80, 6.90, 8.20, 7.60, 5.80, 4.50, 4.30, 3.90, 4.20, 5.70, 7.00, 7.90, 8.20, 7.30, 6.70, 5.50, 5.10, 4.60, 4.7, 5.0, 5.5, 7.2, 7.8, 8.60, 9.40, 10.0, 10.7, 9.9, 9.0, 9.1, 9.3, 9.7, 11.7, 12.3, 12.5, 13.0,              13.9, 14.9, 16, 16.4, 16.8, 10.7, 11.0)     


plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Diagrama ")

#Realizamos el ajuste 

DatosX = x[1:5]; DatosY = y[1:5]
Ajuste_Polinomio = poly.calc(DatosX,DatosY)
Ajuste_Polinomio
DatosX1 = x[4:7]; DatosY1 = y[4:7]
Ajuste_Polinomio1 = poly.calc(DatosX1,DatosY1)
Ajuste_Polinomio1
DatosX2 = x[7:9]; DatosY2 = y[7:9]
Ajuste_Polinomio2 = poly.calc(DatosX2,DatosY2)
Ajuste_Polinomio2
DatosX3 = x[9:10]; DatosY3 = y[9:10]
Ajuste_Polinomio3 = poly.calc(DatosX3,DatosY3)
Ajuste_Polinomio3
DatosX4 = x[10:15]; DatosY4 = y[10:15]
Ajuste_Polinomio4 = poly.calc(DatosX4,DatosY4)
Ajuste_Polinomio4
DatosX5 = x[15:19]; DatosY5 = y[15:19]
Ajuste_Polinomio5 = poly.calc(DatosX5,DatosY5)
Ajuste_Polinomio5
DatosX6 = x[19:24]; DatosY6 = y[19:24]
Ajuste_Polinomio6 = poly.calc(DatosX6,DatosY6)
Ajuste_Polinomio6
DatosX7 = x[24:28]; DatosY7 = y[24:28]
Ajuste_Polinomio7 = poly.calc(DatosX7,DatosY7)
Ajuste_Polinomio7
DatosX8 = x[28:29]; DatosY8 = y[28:29]
Ajuste_Polinomio8 = poly.calc(DatosX8,DatosY8)
Ajuste_Polinomio8
DatosX9 = x[29:32]; DatosY9 = y[29:32]
Ajuste_Polinomio9 = poly.calc(DatosX9,DatosY9)
Ajuste_Polinomio9
DatosX10 = x[32:36]; DatosY10 = y[32:36]
Ajuste_Polinomio10 = poly.calc(DatosX10,DatosY10)
Ajuste_Polinomio10
DatosX11 = x[36:41]; DatosY11 = y[36:41]
Ajuste_Polinomio11 = poly.calc(DatosX11,DatosY11)
Ajuste_Polinomio11
DatosX12 = x[41:45]; DatosY12 = y[41:45]
Ajuste_Polinomio12 = poly.calc(DatosX12,DatosY12)
Ajuste_Polinomio12
DatosX13 = x[45:50]; DatosY13 = y[45:50]
Ajuste_Polinomio13 = poly.calc(DatosX13,DatosY13)
Ajuste_Polinomio13
DatosX14 = x[50:52]; DatosY14 = y[50:52]
Ajuste_Polinomio14 = poly.calc(DatosX14,DatosY14)
Ajuste_Polinomio14
DatosX15 = x[52:55]; DatosY15 = y[52:55]
Ajuste_Polinomio15 = poly.calc(DatosX15,DatosY15)
Ajuste_Polinomio15
DatosX16 = x[55:56]; DatosY16 = y[55:56]
Ajuste_Polinomio16 = poly.calc(DatosX16,DatosY16)
Ajuste_Polinomio16
DatosX17 = x[56:60]; DatosY17 = y[56:60]
Ajuste_Polinomio17 = poly.calc(DatosX17,DatosY17)
Ajuste_Polinomio17
DatosX18 = x[59:62]; DatosY18 = y[59:62]
Ajuste_Polinomio18 = poly.calc(DatosX18,DatosY18)
Ajuste_Polinomio18
DatosX19 = x[62:64]; DatosY19 = y[62:64]
Ajuste_Polinomio19 = poly.calc(DatosX19,DatosY19)
Ajuste_Polinomio19
#ingresamos los ajustes y los puntos en la grafica
plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
points(DatosX,DatosY, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio,add=T,from =14.6,to =14.8)
points(DatosX1,DatosY1, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio1,add=T,from =14.8,to =15.7)
points(DatosX2,DatosY2, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio2,add=T,from =15.7,to =17.6)
points(DatosX3,DatosY3, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio3,add=T,from =17.5,to =17.6)
points(DatosX4,DatosY4, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio4,add=T,from =14.4,to =17.6)
points(DatosX5,DatosY5, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio5,add=T,from =14.4,to =15.1)
points(DatosX6,DatosY6, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio6,add=T,from = 14, to = 15.1)
points(DatosX7,DatosY7, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio7,add=T,from = 14, to = 13)
points(DatosX8,DatosY8, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio8,add=T,from = 13.3, to = 13)
points(DatosX9,DatosY9, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio9,add=T,from = 12.9, to = 13.3)
points(DatosX10,DatosY10, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio10,add=T,from = 11.6, to = 12.9)
points(DatosX11,DatosY11, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio11,add=T,from = 10.58, to = 11.6)
points(DatosX12,DatosY12, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio12,add=T,from = 9.3, to = 10.58)
points(DatosX13,DatosY13, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio13,add=T,from = 9.3, to = 10.3)
points(DatosX14,DatosY14, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio14,add=T,from =8.6, to = 10.3)
points(DatosX15,DatosY15, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio15,add=T,from =6.65, to = 8.6)
points(DatosX16,DatosY16, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio16,add=T,from =6.6, to = 6.7)
points(DatosX17,DatosY17, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio17,add=T,from =6.6, to = 8.5)
points(DatosX18,DatosY18, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio18,add=T,from =8.5, to = 9.3)
points(DatosX19,DatosY19, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Mano derecha")
curve(Ajuste_Polinomio19,add=T,from =9.3, to = 10.4)
