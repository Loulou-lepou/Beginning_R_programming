# Putting pebbles
# E4 : Đặt sỏi, Tin học trẻ quốc gia 2021, vòng sơ loại - Bảng A
# https://ucode.vn/contests/tin-hoc-tre-quoc-gia-2021-so-khao-bang-a-replay-16562?u=10682&l=16562
# Initially [Round 0], one puts 2 pebbles at a certain distance d > 0 on a straight line.
# Then, [Round 1] one puts another pebble at the midpoint of these pebbles.
# Keep putting pebbles at midpoints of previous consecutive pebbles.
# Input: a natural number N = # round
# Constraint: N <= 10^9
# Output: a unique number is the final digit of # pebbles after round N

# failed 5/7
# N = 1000 => In print(a%%10) : probable complete loss of accuracy in modulus
# N <- as.integer(readline(prompt = "N = "))
# a <- 2
# d <- 1
# for (i in 1: N){
#   d = d * 2
#   a = d + 1
# }
# print(a %% 10)

# succeeded 7/7
# The final digit repeats in a cycle of [3, 5, 9, 7]
# where N in [1, 2, 3, 0] (mod 4)
N <- as.integer(readline(prompt = "N = "))
r <- N %% 4
print(paste('after round ', N, 'the final digit of # pebbles is '))
if (N > 0){
  if (r == 0) print(7) 
  if (r == 1) print(3)
  if (r == 2) print(5)
  if (r == 3) print(9)
} else {print(2)}
