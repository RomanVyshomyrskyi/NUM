interpolation_polynomial <- function(x, fx) {
  n <- length(x)
  A <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      A[i, j] <- x[j]^(i - 1)  # x_j^(i-1)
    }
  }
  
  # Розв’язання системи Aa = f(x)
  a <- solve(A, fx)
  
  return(a)  # коефіцієнти полінома від a₀ до aₙ
}


##Exemple of usege

#x <- c(1, 2, 3)
#fx <- c(2, 3, 5)

#coefficients <- interpolation_polynomial(x, fx)
#print(coefficients)