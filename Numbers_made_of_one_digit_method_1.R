# given a number n
# count all numbers made of one digit that are less than or equal to n

n.1 <- readline(prompt = " n = ")
n.0 <- substring(n.1, 1, 1)
k = nchar(n.1)
n.2 = paste(rep(n.0, k), collapse ="")
n.0 = as.integer(n.0)
if (as.integer(n.2) <= as.integer(n.1)){
  print( 9 * (k - 1) + n.0)
} else {
  print(9 * (k - 1) + n.0 - 1)
}
