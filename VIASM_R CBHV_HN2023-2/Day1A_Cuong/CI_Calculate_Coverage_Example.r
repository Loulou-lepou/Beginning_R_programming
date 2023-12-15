#Generate normally distributed data with mean 23 and variance 4
mu <- 23
sigma <- sqrt(4)
n <- 10
alpha <- 0.05

N <- 100

plot(c(1,N),c(23,23),ylim=c(21,25),xlim=c(1,N),type="l",col="red",las=1,xlab="",ylab="")
for (k in 1:N)
{
  x <- rnorm(n,mu,sigma)
  #Construct a 100(1-alpha)% confidence interval for the mean
  # qnorm(1 - alpha/2) = z_{alpha/2}
  z <- qnorm(1-alpha/2)
  l <- mean(x) - z*sigma/sqrt(n)   # xbar - z_{alpha/2}*sigma/sqrt(n)
  u <- mean(x) + z*sigma/sqrt(n)   # xbar + z_{alpha/2}*sigma/sqrt(n)
  
  cat("A Confidence Interval for mu is [",l,",",u,"]\n")
  if((mu<=u)&(l<=mu))
  {
    cat("The CI contains the true mean value\n")
    lines(c(k,k),c(l,u),col="black")
  }
  else
  {
    cat("The CI DOES NOT contain the true mean value\n")
    lines(c(k,k),c(l,u),col="red",lwd=3)
  }
}

###################################################
# Coverage of CI when variance is known
###################################################
c <- 0
for (N in 1:10000)
{
  x <- rnorm(n,mu,sigma)
  z <- qnorm(1-alpha/2)
  l <- mean(x) - z*sigma/sqrt(n)   
  u <- mean(x) + z*sigma/sqrt(n) 
    
    c <- c+((mu<=u)&(l<=mu))
}
cat("The empirical coverage of these CI is ",c/10000,"\n")
#Note that the empirical coverage is close to 1-alpha.

###################################################
#Coverage of CI when variance is unknown, but we simply plug-in an estimate (wrong approach)
###################################################
c <-0
n <- 15
for (N in 1:10000)
{
	  x <- rnorm(n,mu,sigma)
	  z <- qnorm(1-alpha/2)
    l <- mean(x) - z*sd(x)/sqrt(n)
    u <- mean(x) + z*sd(x)/sqrt(n)
    
    c <- c+((mu<=u)&(l<=mu))
}
cat("The empirical coverage of these CI is ",c/10000, "\n")
#Note that the empirical coverage is less than 1-alpha.

###################################################
#Coverage of CI when variance is unknown, and we use the proper approach with t-quantiles (correct approach)
###################################################
c <- 0
for (N in 1:10000)
{
	  x <- rnorm(n,mu,sigma)
	  t <- qt(1- alpha/2, df = n-1)
    l <- mean(x) - t*sd(x)/sqrt(n)
    u <- mean(x) + t*sd(x)/sqrt(n)
    
    c <- c+((mu<=u)&(l<=mu))
}
cat("The empirical coverage of these CI is ",c/10000,"\n")
#Note that the empirical coverage is close to 1-alpha.

