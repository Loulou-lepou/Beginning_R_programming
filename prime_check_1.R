n <- as.integer(readline(prompt =" Enter a number : "))
check <- 0
if (n > 1){
  check <- 1
  for (i in 2:(n - 1)){
    if ((n %% i) == 0){
      check <- 0
      break
    }
  }
}
if (n == 2){check <- 1}
if (check == 0){
  print(paste(n, 'is not a prime number'))
} else {
  print(paste(n, 'is a prime number'))
}
