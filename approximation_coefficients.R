approximation_coefficients <- function(f, phi_list, lower, upper) {
  n <- length(phi_list)
  A <- matrix(0, n, n)
  b <- numeric(n)
  
  # Обчислення матриці A (внутрішніх добутків базисних функцій)
  for (i in 1:n) {
    for (j in 1:n) {
      integrand <- function(x) phi_list[[i]](x) * phi_list[[j]](x)
      A[i, j] <- integrate(integrand, lower, upper)$value
    }
  }
  
  # Вектор правої частини b (добутки f з базисом)
  for (i in 1:n) {
    integrand <- function(x) f(x) * phi_list[[i]](x)
    b[i] <- integrate(integrand, lower, upper)$value
  }
  
  # Розв’язання системи Ac = b
  c <- solve(A, b)
  return(c)  # коефіцієнти c₀, ..., cₙ
}


## Exemple of use

# Функція, яку апроксимуємо
#f <- function(x) exp(x)

# Базисні функції (поліномний базис до степеня 2)
#phi_list <- list(
#  function(x) 1,
#  function(x) x,
#  function(x) x^2
#)

# Інтервал апроксимації
#lower <- 0
#upper <- 1

#coefficients <- approximation_coefficients(f, phi_list, lower, upper)
#print(coefficients)