lagrange_interpolation <- function(x_points, f_values, x) {
  n <- length(x_points)
  Ln <- 0
  
  for (i in 1:n) {
    xi <- x_points[i]
    li_num <- 1
    li_den <- 1
    
    for (j in 1:n) {
      if (j != i) {
        xj <- x_points[j]
        li_num <- li_num * (x - xj)
        li_den <- li_den * (xi - xj)
      }
    }
    
    li <- li_num / li_den
    Ln <- Ln + li * f_values[i]
  }
  
  return(Ln)
}

## Exemple of use

#x_nodes <- c(1, 2, 3)
#f_nodes <- c(2, 3, 5)
#x_eval <- 2.5

#Ln_value <- lagrange_interpolation(x_nodes, f_nodes, x_eval)
#print(Ln_value)