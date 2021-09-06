# find the largest digit of a given number n
# ---------------------------------------------------------
# Method 1: accept the input string n
# ---------------------------------------------------------
largest.digit.1 <- function( n)
{
  largest<- 0
  for (i in 1:nchar(n))
  {
    if (largest < substring(n,i,i)) {largest <- substring(n,i,i)}
  }
  
  return (largest)
}


# ---------------------------------------------------------
# Method 1: accept the input integer n
# ---------------------------------------------------------
largest.digit.2 <- function(n)
{ 
  largest <- 0
  while (n > 0)
  {
    last.digit <- n %% 10
    if (largest < last.digit){largest <- last.digit}
    n <- n %/% 10
  }
  return (largest)  
}


main <- function()
{
  num <- readline(prompt = 'n = ')
  print(paste('the largest digit of', num, '=', largest.digit.1(num)))
  print(paste('the largest digit of 1093832 = ', largest.digit.2(1093832)))
}


main()