data.set <- c(56, 45, 23, 54, 56, 
              12, 43, 65, 76, 23,
              54, 64, 34, 34, 23,
              21, 11, 9)

# 1. MEAN
# compute the mean using mean() function
mean.data.set <- mean(data.set)
print(round(mean.data.set, digits=5))

# using sum() function to calculate the mean 

print(sum(data.set)/ length(data.set))

# using for loop to calculate the mean
total <- 0
n <- length(data.set)
for (i in 1:n){
  total <- total + data.set[i]
}
print(total / n)

# 2. SAMPLE VARIANCE
print(paste("variance of the data set = ", 
            var(data.set)))

print(paste("standard deviation of the data set =",
             sd(data.set), 
             " = ", sqrt(var(data.set))))

# using mean() function to calculate the sample variance

minus.mean <- data.set - mean.data.set
minus.mean.square <- minus.mean^2

# print(minus.mean)
# print(minus.mean.square)
print(mean(minus.mean.square)* n / (n - 1))

# using ur own mean() function
my.mean <- function(column.vector){
  total <- 0
  n <- length(column.vector)
  for (i in 1:n){
    total <- total + column.vector[i]
  }
  return (total / n)
}

print(paste("using my.mean(), mean of the data set = ", 
            my.mean(data.set)))

print(my.mean(minus.mean.square) * n / (n - 1))

