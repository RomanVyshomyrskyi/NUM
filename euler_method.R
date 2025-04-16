euler_method <- function(x, f, y0) {
  n <- length(x)
  y <- numeric(n)
  y[1] <- y0
  
  for (i in 1:(n - 1)) {
    h <- x[i + 1] - x[i]
    y[i + 1] <- y[i] + h * f(x[i], y[i])
  }
  
  return(y)
}


##Exemple of usege

#f <- function(x, y) x + y
#x_vals <- seq(0, 1, length.out = 11)  # рівномірна сітка з кроком 0.1
#y0 <- 1

#y_vals <- euler_method(x_vals, f, y0)

# Виведення результату:
#data.frame(x = x_vals, y = y_vals)