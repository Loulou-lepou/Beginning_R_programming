# Counting disks
# Inputs
# line 1 : a natural number N = the amount of water (L) to be poured down 
# line 2 : a natural number a = capacity of disk 1
# line 3 : a natural number b = the amount of water (l) that successive disk can 
# contain more than the previous disk
# Constraints : N <= 10^16, a <= 100, b <= 10
# Output : a unique integer number c = # disks that contain water

N <- as.integer(readline(prompt ='the amount of water to be poured down = '))
a <- as.integer(readline(prompt ='capacity of 1st disk = '))
b <- as.integer(readline(prompt ='difference in capacity of 2 consecutive disks = '))

count <- 0
while (N > 0) {
  count <- count + 1
  N <- N - a
  a <- a + b
}
print(count)
