krylov_method <- function(A) {
  n <- nrow(A)
  y <- matrix(0, n, 1)
  y[1, 1] <- 1  # одиничний вектор (1, 0, ..., 0)^T
  
  A_powers <- list()
  A_prev_y <- y
  
  # Обчислюємо A^j y для j = 0 до n-1
  for (j in 1:n) {
    A_powers[[j]] <- A_prev_y
    A_prev_y <- A %*% A_prev_y
  }
  
  # Формуємо матрицю з векторів A^{j-1} y
  A_tilde <- do.call(cbind, A_powers)
  
  # Обчислюємо -A^n y
  b_tilde <- -A %*% A_prev_y
  
  # Розв’язуємо систему A_tilde %*% b = b_tilde
  b <- solve(A_tilde, b_tilde)
  
  return(list(coefficients = b))  # це b₀, ..., b_{n−1}
}
## exemple of usege

#A <- matrix(c(2, 1, 0,
#              0, 2, 1,
#              0, 0, 2), nrow = 3, byrow = TRUE)

#result <- krylov_method(A)
#print(result$coefficients)