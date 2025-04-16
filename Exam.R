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


plot(i_vals, y_vals, type = "b", pch = 19, col = "blue",
     xlab = "i", ylab = expression(y(i) == I[N](i) - I[A]))
abline(h = 0, col = "red", lty = 2)

print(y_vals)



