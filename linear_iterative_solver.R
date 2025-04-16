iterative_solver <- function(M, N, b, x0, epsilon) {
  xk <- x0
  k <- 0
  gk <- M %*% xk - N %*% b - xk
  
  while (sqrt(sum(gk^2)) >= epsilon * sqrt(sum(b^2))) {
    xk <- M %*% xk - N %*% b
    gk <- M %*% xk - N %*% b - xk
    k <- k + 1
  }
  
  return(list(xk = xk, iterations = k))
}

# Exemple of usege
# Матриця A і вектор b
#A <- matrix(c(4, 1, 2,
#              3, 5, 1,
#              1, 1, 3), nrow = 3, byrow = TRUE)
#b <- matrix(c(4, 7, 3), ncol = 1)

# Побудова D, R, M, N для методу Якобі
#D <- diag(diag(A))
#R <- A - D
#D_inv <- solve(D)
#M <- -D_inv %*% R
#N <- -D_inv

# Початкове наближення
#x0 <- matrix(c(0, 0, 0), ncol = 1)

# Виклик методу
#result <- iterative_solver(M, N, b, x0, epsilon = 1e-6)
#print(result)