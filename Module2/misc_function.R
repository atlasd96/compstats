fixed_point_iteration <- function(x0, fprime, alpha=1, max_iter=100000, tol=.001){
  n <- 0
  xt <- x0
  while((n < 1 & n < max_iter) | abs(x0 - xt) > tol){
    # Update the point
    x0 <- xt
    
    # Define update equation
    xt <- x0 + (alpha * fprime(x0))
    
    n <- n + 1
  }
  print(paste0("The solution is: ", xt, ". The algorithm converged in ", n, " iterations"))
  return(xt)
}

secant_method <- function(x0, x1, fprime, max_iter=100000, tol=.001){
  n <- 0
  
  while((n < 1 & n < max_iter) | abs(x1 - x0) > tol){
    # Define the updating equation
    xt <- x1 - fprime(x1) * (x1 - x0) / (fprime(x1) - fprime(x0))
    
    # Iterate on update
    x1 <- xt; x0 <- x1; 
    
    n <- n + 1
  }
  print(paste0("The solution is: ", xt, ". The algorithm converged in ", n, " iterations"))
  return(xt)
}