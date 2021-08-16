f <- numeric(10)
f[1] <- f[2] <- 1
for (i in 3:10){
  f[i] <- f[i - 2] + f[i - 1]
}
print('First 10 Fibonacci numbers')
print(f)
