# check if the input number is prime or not

# Step 1 : Take input from the user 
# readline(prompt ="..." )  -> read the input and return a string
# as.integer() -> convert the input string to an integer
n <- as.integer(readline(prompt =" Enter a number : "))

# Step 2: check == 0 -> not a prime,  check == 1 -> a prime
check <- 0                # Initially, n = 0, 1 -> check = 0
if (n == 2){check <- 1}
if (n > 2){
  check <- 1              # n > 2 :  initially, check = 1
  for (i in 2:floor(sqrt(n))){
    if ((n %% i) == 0){
      check <- 0          # i divides n -> n is composite
      break               # find a divisor -> break out of the for loop
    }
  }
}

if (check == 0){
  print(paste(n, 'is not a prime number'))
} else {
  print(paste(n, 'is a prime number'))
}
