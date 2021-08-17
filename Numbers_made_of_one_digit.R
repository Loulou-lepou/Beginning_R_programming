# Numbers made of one digit: 1, 11, 555, 99999
# Input : line 1 contains a natural number N
# Constraint : N <= 10^9
# Output: # numbers made of 1 digit that are less than or equal to N

N.1 <- readline(prompt="N = ")
N <- as.integer(N1)
# N0 is the first digit of N
N.0 <- as.integer(substr(N1, 1,1))
# l = # digits of N
l <- nchar(N.1)

# max = N0N0N0....N0  (l digits N0)
max <- N.0 * (10 ^ l - 1) / 9

# # k-digit numbers made of 1 digit 
# = # {11...1, 22...2, 33...3,...,99...9} = 9
print(paste('# numbers made of 1 digit <= ', N, 'is'))
if (max < N){ 
  print( 9 * (l - 1) + N.0 ) 
} else {
  print(9 * (l - 1) + N.0 - 1 )
}
