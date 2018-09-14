library(ggplot2)
library(RColorBrewer)

oil <- read.table("../datasets/oilspills.dat", header=TRUE)

fprime <- function(alpha, b, x){
  return(c(sum(x * b[, 1] / (b %*% alpha)) - sum(b[, 1]), sum(x * b[, 2] /  (b %*% alpha)) - sum(b[, 2])))
}

f2prime <- function(alpha, b, x){
  return(-1 * matrix(c(
    sum(x * b[, 1]^2 / (b %*% alpha)^2),
    sum(x * apply(b, 1, prod) / (b %*% alpha)^2),
    sum(x * apply(b, 1, prod) / (b %*% alpha)^2),
    sum(x * b[, 2]^2 / (b %*% alpha)^2)
  ), ncol=2))
}


newtons_method <- function(fprime, f2prime, alpha0, b, x, max_iter=10000, tol=.001){
  alpha_t <- alpha0
  
  # Iterate through 
  for (n in 1:max_iter){
    # Set stopping conditions
    if(sum((alpha0 - alpha_t)^2) < tol & n > 1){break}
    
    alpha0 <- alpha_t
    
    # Get the Newton update
    alpha_t <- alpha0 - solve(f2prime(alpha0, b, x)) %*% fprime(alpha0, b, x) 
  }
  
  return(c(alpha_t=alpha_t, n=n))
}

# We call the function on our dataset
alpha0 <- c(1, 1)
b <- as.matrix(oil[, c('importexport', 'domestic')])
x <- as.matrix(oil[, c('spills')])

solution <- newtons_method(fprime, f2prime, c(1, 1), b, x, tol=.00001)

log_likelihood <- function(alpha, b, x){
  return(sum(x * log(b %*% alpha)) - sum(alpha[1] * b[, 1]) - sum(alpha[2] * b[, 2]) - sum(log(factorial(x))))
}

# we construct agrid of the likelihood function to plot the contours
a1 <- seq(0.1, 2, .01)
a2 <- seq(0.1, 2, .01)
alpha_space <- as.matrix(expand.grid(a1, a2)) # Create cartesian product

# Find the likelihod for all pairs
results <- data.frame(cbind(alpha_space, apply(alpha_space, 1, function(alpha) log_likelihood(alpha, b=b, x=x))))
colnames(results) <- c("alpha1", "alpha2", "likelihood") # Add column names

# Plot the contours with the solution in red
ggplot(results) + 
  geom_contour(aes(x=alpha1, y=alpha2, z=likelihood), bins=1000) + 
  geom_point(aes(x=solution[1], y=solution[2]), colour="red")
  
