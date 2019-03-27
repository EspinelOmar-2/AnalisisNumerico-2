library(pracma)
library(PolynomF)
library(Matrix)
library(rSymPy)
rm(list=ls())

x = c(35,45,55,65,75)
y = c(35,48,70,40,22)

plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Rango de notas por ajuste de Lagrange ")


DatosX = x[1:5]; DatosY = y[1:5]
polinomio = poly.calc(DatosX, DatosY)
polinomio
##curve(polinomio,add=T,from =35,to =75)


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

lag<-lagrange.poly(x,y)
lag
fun<- function(x)495831/128 - 37253*x/120 + 43679*x^2/4800 - 343*x^3/3000 + x^4/1920
curve(fun,add=T,from =35,to =75)