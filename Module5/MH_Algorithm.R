set.seed(73)

f <- function(x, lambda=.7) lambda * dnorm(x, 7, .5^2) + (1 - lambda) * dnorm(x, 10, .5^2)
stddev <- 1
g <- function(x, x0) dnorm(x, mean=x0, sd=stddev)
rg <- function(x0) rnorm(n=1, mean=x0, sd=stddev)

find_xt <- function(xstar, xt, mh_ratio){
  cutoff <- runif(n=1, min=0, max=1)
  return(ifelse(cutoff < min(mh_ratio, 1), xstar, xt))
}

metropolis_hastings <- function(g, rg,  f, x0, iterations=10000){
  path <- rep(0, iterations)
  for (i in 1:iterations){
    xstar <- rg(x0)
    mh_ratio <- (f(xstar) * g(x0, xstar)) / (f(x0) * g(xstar, x0))
    
    x0 <- find_xt(xstar, x0, mh_ratio)
    path[i] <- x0
  }
  return(path)
}

x0 <- 7
iterations <- 100000
path <- metropolis_hastings(g=g, rg=rg, f=f, x0=x0, iterations=iterations)
plot(seq(1, iterations), path)
hist(path)


