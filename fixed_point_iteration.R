fixed_point_iteration <- function(G, x0, epsilon) {
  x_prev <- x0
  x_curr <- G(x_prev)
  k <- 1
  
  while (abs(x_curr - x_prev) >= epsilon) {
    x_prev <- x_curr
    x_curr <- G(x_prev)
    k <- k + 1
  }
  
  return(list(xk = x_curr, iterations = k))
}


#Example usage:
#G <- function(x) cos(x)  # Example function
#result <- fixed_point_iteration(G, x0 = 1, epsilon = 1e-6)
#print(result)