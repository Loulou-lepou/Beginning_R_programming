# print 10 first Fibonacci numbers
# initialize f as a numeric vector of length 10
f <- numeric(10)
# first and second element of f = 1
f[1] <- f[2] <- 1
# find next Fibonacci numbers by using the recursive relation 
for (i in 3:10){
  f[i] <- f[i - 2] + f[i - 1]
}
# print the result
print('First 10 Fibonacci numbers')
print(f)
