midpoint_integral <- function(f, a, b, n) {
  h <- (b - a) / n
  I <- 0
  
  for (i in 0:(n - 1)) {
    xi <- a + i * h + h / 2  # середина підінтервалу
    Si <- h * f(xi)
    I <- I + Si
  }
  
  return(I)
}

##Exemple of use

#f <- function(x) x^2
#a <- 0
#b <- 1
#n <- 10

#result <- midpoint_integral(f, a, b, n)
#print(result)  # Очікуване значення ~ 1/3 ≈ 0.333...