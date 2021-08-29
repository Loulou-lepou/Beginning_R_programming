# Given n, list all primes <= n
# Ref links:
# 1. https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
# 2. https://www.javainterviewpoint.com/python-sieve-of-eratosthenes/
# 3. https://www.geeksforgeeks.org/sieve-of-eratosthenes/
# Pseudocode:
# algorithm Sieve of Eratosthenes
# input: an integer n > 1
# Output: all prime numbers from 2 through n
# let A be an array of Boolean values, indexed by integers 2 to n
# initially all set to be true
# for i = 2.. floor(sqrt(n)) do
# if A[i] is true
#      for j = i^2.. n do
#            A[j] := false
# return all i such that A[i] is true
# Time complexity: O(n*(log(log n)))
# given array update is O(1) operation


sieve.of.eratosthenes.1 <- function(n)
{
  # c{base} combines values into a vector
  # rep{base} replicate elements of vectors
  # basic logical constants T = TRUE, F = FALSE
  # x = a vector of consecutive numbers from 1 to n
  x <- c(rep(T, n))
  
  # seq(from, to, by =)
  for (i in seq(3, floor(sqrt(n)), 2))
  {
    if (x[i])  # 'i' is a prime
    {
      # initially list odd numbers i, mark only odd multiples from i^2
      for (j in seq(i^2, n, 2*i)) x[j] <- F
    }
  }
  prime.list <-c(2)
  
  for (i in seq(3, n, 2))
  {
    if (x[i]){prime.list <- append(prime.list, i)}
  }
  return (prime.list)
}


main <- function()
{
  n1 <- as.integer(readline(prompt = " N = ")) 
  library(tictoc)
  tic(paste("Running time with n = ", n1, ' is ', collapse = ''))
  print(sieve.of.eratosthenes.1(1000))
  toc()
}


main()
