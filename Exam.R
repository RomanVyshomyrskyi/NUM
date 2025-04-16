I_analyticke <- log(2)

f <- function(x) {
  1 / x
}

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


i_val <- 0:20
y_val <- numeric(length(i_val))

for (j in seq_along(i_val)) {
  i <- i_val[j]
  n <- 2^i
  I_numericke <- midpoint_integral(f, 1, 2, n)
  y_val[j] <- I_numericke - I_analyticke
}


plot(i_val, y_val, type = "b", pch = 19, col = "blue",
     xlab = "i", ylab = expression(y(i) == I[N](i) - I[A]))
abline(h = 0, col = "red", lty = 2)

print(y_val)


least_squares_fit <- function(x, y) {
  
  x_mean <- mean(x)
  y_mean <- mean(y)
  
  b <- sum((x - x_mean) * (y - y_mean)) / sum((x - x_mean)^2)
  a <- y_mean - b * x_mean
  
  y_fit <- a + b * x
  
  plot(x, y, main = "Manual Least Squares Fit", xlab = "x", ylab = "y", pch = 19)
  
  
  lines(x, y_fit, col = "green", lwd = 2)
  
  eq <- paste0("y = ", round(a, 2), " + ", round(b, 2), " * x")
  legend("topleft", legend = eq, bty = "n", col = "green", lwd = 2)
  
  return(list(intercept = a, slope = b))
}

least_squares_fit(i_val, y_val)

