library(latex2exp)

range0 <- function(avector){
  return(max(avector) - min(avector))
}

monte_carlo_integration <- function(f, a, b, n, plot=FALSE){
  
  # Get the range of the function on the space
  f_range <- range(f(seq(a, b, .01))) 
  
  total_area <- range0(f_range) * (b - a)
  print(paste0("Total Area: ", total_area))
  
  x_points <- runif(n, a, b)
  y_points <- runif(n, f_range[1], f_range[2])
  
  proportion_under <- mean(f(x_points) >= y_points)
  print(paste0("Proportion Under: ", proportion_under))
  
  if(plot){
    plot(seq(a, b, .01), f(seq(a, b, .01)), 'l', lwd=10, col='blue', xlab='X', ylab=TeX("$f(x)$"))
    points(x_points, y_points, col= as.factor(ifelse(f(x_points) <= y_points, 0, 1)))
    
  }
  
  return(total_area * proportion_under)
  
  
}


f <- function(x) 4 * sqrt(1 - x ^ 2)
monte_carlo_integration(f, 0, 1, 10000, plot=TRUE)


