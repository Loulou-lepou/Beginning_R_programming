de.polignac <- function(n, p) {
  k.max <- floor(log(n) / log(p))
  first.term <- floor( n / p)
  pow <- first.term
  if (1 < k.max) {
    for (i in (2: k.max)){
      first.term <- floor(first.term / p)
      pow <- pow + first.term
    }
  }
  pow
}

print(paste('the highest power of ', 
            5, 'in 1000! = ',
            de.polignac(1000, 5)))
