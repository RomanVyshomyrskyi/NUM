newton_system <- function(F, J, x0, epsilon) {
  x_prev <- x0
  delta_x <- solve(J(x_prev), -F(x_prev))  # Розв’язуємо систему J(x) * dx = -F(x)
  x_curr <- x_prev + delta_x
  k <- 1
  
  while (sqrt(sum((x_curr - x_prev)^2)) >= epsilon) {  # Евклідова норма
    x_prev <- x_curr
    delta_x <- solve(J(x_prev), -F(x_prev))
    x_curr <- x_prev + delta_x
    k <- k + 1
  }
  
  return(list(xk = x_curr, iterations = k))
}

## Exemole of usege

#F <- function(x) {
#  c(x[1]^2 + x[2]^2 - 4,
#    x[1] - x[2])
#}

#J <- function(x) {
#  matrix(c(2 * x[1], 2 * x[2],
#           1,       -1), 
#         nrow = 2, byrow = TRUE)
#}

#x0 <- c(1, 1)
#epsilon <- 1e-6

#result <- newton_system(F, J, x0, epsilon)
#print(result)