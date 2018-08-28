## Problem 1.
# Read in the data
data <- read.table('./data/favorite.data', colClasses = c('numeric'))[, 1]

# Print the required summary statistics
# a)
print(paste0("The mean is ", mean(data)))
# b)
print(paste0("The median is ", median(data)))
# c)
print(paste0("The standard deviation is ", sd(data)))
# d)
print(paste0("The max is", max(data)))
# e)
print(paste("The min is", min(data)))

# Generate a PDF histogram
# f)
pdf("problem1_histogram.pdf", height=8.5, width=14)
hist(data, main='Histogram of favorite.data', xlab='Value')
dev.off()

## Problem 2.
set.seed(73) # set a seed for reproducibility
# Generate 10,000 standard normal data points
normal_data <- rnorm(n=10000, mean=0, sd=1)

# Generate the histogram of values
# a)
pdf("problem2_histogram.pdf", height=8.5, width=14)
hist(normal_data, main='Histogram of Standard Normal Data', xlab='Value')
dev.off()

# b)
# Print out the mean, median and standard deviation here
# We use a vectorized functional approach
print(sapply(list("mean", "median", "sd"), function(f){
  paste(f, round(do.call(f, list(normal_data)), 5), sep=": ")
}))

## Problem 3.
a <- seq(5, 160, 5)
b <- seq(87, 56, -1)
d <- a * b 
# a)
# We find the 15th, 16th and 17th element of d
print(sapply(list(15, 16, 17), function(i){
  paste0("The ", i, "th element of d is ", d[i])
}))

# b) 
# We find the elements of d that are greater than 2000
print("The following elements are the indices of d corresponding to elements greater than 2000")
seq(1, length(d))[d > 2000]

# c) 
# We find the number of elements that are greater than 6000
print(paste(sum(d > 6000), "elements of d are greater than 6000"))

## Problem 4
# a)
add_perfect_squares <- function(x){
  perfect_squares <- 0
  # Loop through all elements between 1 and x
  for (z in seq(1, x)){
    # If x is a perfect square
    if (sqrt(z) %% 1 == 0){
      # add it to the vector
      perfect_squares <- perfect_squares + z
    }
  }
  return(sum(perfect_squares))
}

# a)
print("The sum of all perfect squares between 1 and: ")
print(paste(100, add_perfect_squares(100), sep=": "))
# b)
print(paste(100000, add_perfect_squares(100000), sep=": "))







