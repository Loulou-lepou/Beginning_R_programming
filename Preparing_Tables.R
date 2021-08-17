# Problem : Preparing tables
# Input : line 1 : N = # candidates
#         line 2 : K = maximum number of candidates per table
# Constraints : K <= N <= 10^9
# Outputs : unique number  = minimum number of tables

N <- as.integer(readline(prompt = "# candidates :"))
K <- as.integer(readline(prompt ="The maximum number of candidates per table :"))
if (K <= N & K <= 10^9){
  if (N %% K == 0) {
    print(paste(" minimum number of tables = ", N %/% K))
  } else{
    print(paste("minimum number of tables = ", N %/% K + 1))
  }
}
