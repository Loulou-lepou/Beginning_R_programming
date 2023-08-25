de.polignac <- function(n, p){
  term <- floor(n / p)
  pow <- 0
  while (term > 0){
    pow <- pow + term
    term <- floor(term / p)
    # print(paste("next term = ", term, "sum = ", pow))
  }
  
  return (pow)
}


print(de.polignac(30, 3))

# source("C:\Users\Admin\Desktop\de_polignac_2.R", print.eval=T)
# source("C:\\Users\\Admin\\Desktop\\de_polignac_2.R", print.eval=T)
