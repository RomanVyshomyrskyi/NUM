LSA <- function(x, y, n) {
  X <- outer(x, 0:(n-1), "^")
  a <- solve(t(X) %*% X, t(X) %*% y)
  return(a)
}