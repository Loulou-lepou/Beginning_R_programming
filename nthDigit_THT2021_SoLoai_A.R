# -------------------------------------------------------------------
# Problem: Find the nth digit in a string
# -------------------------------------------------------------------
# E5, Tin hoc tre quoc gia 2021, so loai, bang A
# https://ucode.vn/contests/tin-hoc-tre-quoc-gia-2021-so-khao-bang-a-replay-16562?u=10682&l=16562
# Given 2 natural numbers A & N. 
# We set up a string by starting from A, then keep adding successive natural numbers
# that have the same parity as A
# Find the nth digit in the string
# Input : line 1: a natural number A
#         line 2: a natural number N
# Constraint : A, N <= 10^9
# Output: a unique number is the nth character in the string

# -------------------------------------------------------------------
# Method 1: Forming the string, failed 3/7
# -------------------------------------------------------------------
# it takes a long time to 
# 1) form & store the string 
# 2) convert datatypes b/w string & integer
# 3) search & find the nth character in the string

# A1 <- readline(prompt = "a = ")
# A <- as.integer(A1) 
# N <- as.integer(readline(prompt = " N = "))
# #  
# l <- nchar(A1)
# while (l < N){
#   A <- A + 2
#   A2 <- toString(A)
#   A1 <- paste(A1, A2, sep="")
#   l <- nchar(A2)
#   N <- N - l
# }
# print(substring(A1,N,N))

# -------------------------------------------------------------------
# Method 2: failed 6/7 (Time limit exceeded)
# -------------------------------------------------------------------
# Improvement compared to method 1:
# don't need to form the whole string
# the underlying string was updated number-by-number

# A1 <- readline(prompt = " a = ")
# A <- as.integer(A1)
# N <- as.integer(readline(prompt = " N = "))
# 
# d <- N - nchar(A1)
# if (d < 0){
#   print(substring(A1,N,N))
# } else{
#   while (d > 0){
#     # pay attention to the substring starting from A + 2
#     # d = distance from the desired character 
#     # to the end character of the new starting point of the substring
#     A <- A + 2
#     A2 <- toString(A)
#     l2 <- nchar(A2)
#     d <- d - l2
#   }
#   # out of the while loop d <= 0
#   # d = 0 => print the final character in string A2
#   # d > 0 => print the dth character in string A2
#   if(d == 0){
#     print(as.integer(substring(A2,l2, l2)))
#   } else{
#     print(as.integer(substring(A2,l2 + d, l2 + d)))
#   }
# }
# 

# -------------------------------------------------------------------
# Method 3: optimal - succeeded 7/7
# -------------------------------------------------------------------
A1 <- readline(prompt = " A = ")
A <- as.integer(A1)
N <- as.integer(readline(prompt = "N = "))

# l = # digits of A
l <- nchar(A1)
# next.min = minimum (l + 1) - digit number
next.min <- 10^l
# n.group = maximum number of groups can be formed by N characters,
#          each group has length = l
n.group <- N %/% l
# k = # l-digit numbers having the same parity as A 
k <- (next.min - A + 1) %/% 2
# d = distance form the desired character 
# to the starting character of the string
d <- N

# if n.group > k, the desired character doesnt belong to 
# the substring A (A + 2)....(A + 2*(k-1))
# consists of k numbers whose number of digits is the same as A = l
# length of this substring = k * l
# After remove this string, we update A = A + 2k as the new starting point
# with # digits = l + 1
# distance from the desired character to the new starting character of 
# the new string d = d - k * l
while (n.group >= k){
  d <- d - k * l
  A <- A + 2 * k
  l <- l + 1
  next.min <- next.min * 10
  k <- (next.min - A) %/% 2
  n.group <- d %/% l
}

# out of the while loop, n.group <= k
# the desired character is in substring A1
# if l divides d, n.group of l-digit numbers
# then (A: l - digit) (A + 2 : l - digit)...(A + 2 * (n.group - 1) : l - digit)
# print the final digit of A1 = A + 2 * (n.group - 1)
# if d %% l = r, n.group of l-digit numbers = d %/% l
# then (A:l-digit) (A+2: l-digit)....(A + 2* (n.group - 1))
# consists of n.group * l digits,  r remaining digits belong to A + 2 * n.group
# print the r th digit of A1 = A + 2 * n.group

r <- d %% l
if (r == 0){
  A1 <- toString(A + 2 * (n.group - 1)) 
  print(as.integer(substring(A1, nchar(A1) , nchar(A1) )))
} else{
  A1 <- toString(A + 2 * n.group)
  print(as.integer(substring(A1, r , r )))
}
