I_analyticke <- log(2)

f <- function(x) {
  1 / x
}

midpoint_integral <- function(f, a, b, n) {
  h <- (b - a) / n
  I <- 0
  
  for (i in 0:(n - 1)) {
    xi <- a + i * h + h / 2 
    Si <- h * f(xi)
    I <- I + Si
  }
  
  return(I)
}


i_val <- 1:24
y_val <- numeric(length(i_val))

for (j in seq_along(i_val)) {
  i <- i_val[j]
  n <- 2^i
  I_numericke <- midpoint_integral(f, 1, 2, n)
  y_val[j] <- I_numericke - I_analyticke
  if (j>1){
    if (abs(y_val[j])> abs(y_val[j-1])){
      print(j)
    }
  }

}


plot(i_val, y_val, type = "b", pch = 19, col = "blue",
     xlab = "i", ylab = expression(y(i) == I[N](i) - I[A]))
abline(h = 0, col = "red", lty = 2)

print(y_val)


least_squares_approx <- function(x, y, basis_functions) {
  # Ensure x and y are vectors of the same length
  if(length(x) != length(y)) {
    stop("x and y must have the same length")
  }
  
  m <- length(x)
  n <- length(basis_functions)
  
  # Initialize design matrix A (m rows, n columns)
  A <- matrix(0, nrow = m, ncol = n)
  
  # Fill matrix: A[i, j] = phi_j(x_i)
  for (j in 1:n) {
    A[, j] <- basis_functions[[j]](x)  # Apply basis function j to all x
  }
  
  # Solve the normal equations: (A^T A) c = A^T y
  coeff <- solve(t(A) %*% A, t(A) %*% y)
  
  return(as.vector(coeff))
}

poly_basis <- list(
  function(x) 1,
  function(x) 1 / (2^x),
  function(x) 1 / x,
  function(x) 1 / (x^2)
)

coeff_poly <- least_squares_approx(i_val, y_val, poly_basis)
print(coeff_poly)  # display the coefficients

Horner <- function(a,x){
  n <- length(a)
  y <- a[n]
  for(i in (n-1):1) y <- y*x+a[i]
  return(y)
}

aprox_y_val <- sapply(i_val, function(xi) {
  sum(mapply(function(c, phi) c * phi(xi), coeff_poly, poly_basis))
})

lines(i_val, aprox_y_val, col = "darkgreen", lwd = 2)
