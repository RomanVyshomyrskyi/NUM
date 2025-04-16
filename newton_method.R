newton_method <- function(f, f_prime, x0, epsilon) {
  x_prev <- x0
  x_curr <- x_prev - f(x_prev) / f_prime(x_prev)
  k <- 1
  
  while (abs(x_curr - x_prev) >= epsilon) {
    x_prev <- x_curr
    x_curr <- x_prev - f(x_prev) / f_prime(x_prev)
    k <- k + 1
  }
  
  return(list(xk = x_curr, iterations = k))
}

## Exemple of usege:

#f <- function(x) x^2 - 2         # Маємо знайти sqrt(2)
#f_prime <- function(x) 2 * x     # Похідна функції

#result <- newton_method(f, f_prime, x0 = 1, epsilon = 1e-6)
#print(result)