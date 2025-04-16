bisection_method <- function(f, a0, b0, epsilon) {
  k <- 0
  ak <- a0
  bk <- b0
  
  while (abs(bk - ak) >= epsilon) {
    sk <- (ak + bk) / 2
    
    if (f(ak) * f(sk) < 0) {
      ak_new <- ak
      bk_new <- sk
    } else if (f(sk) * f(bk) < 0) {
      ak_new <- sk
      bk_new <- bk
    } else {
      ak_new <- sk
      bk_new <- sk
    }
    
    ak <- ak_new
    bk <- bk_new
    k <- k + 1
  }
  
  sk <- (ak + bk) / 2
  return(list(sk = sk, ak = ak, bk = bk, iterations = k))
}