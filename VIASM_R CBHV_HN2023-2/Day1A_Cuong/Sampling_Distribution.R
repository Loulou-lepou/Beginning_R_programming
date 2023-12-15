
# SAMPLE DISTRIBUTION OF SAMPLE MEAN

# Simulations using a Discrete Distribution
x = c(1, 3, 5)
px = c(0.6, 0.3, 0.1)

draws = sample(x, size = 500, replace = TRUE, prob = px)
hist(draws, breaks = seq(1, 5, by = 0.25), main = paste(length(draws), " discrete draws", sep = ""))

# 500 samples, sample size = 4
draws = sample(x, size = 4 * 500, replace = TRUE, prob = px)
draws = matrix(draws, 4)
drawmeans = apply(draws, 2, mean)
hist(drawmeans, breaks = seq(1, 5, by = 0.25), main = "500 means of 4 draws")

# 500 samples, sample size = 16
draws = sample(x, size = 16 * 500, replace = TRUE, prob = px)
draws = matrix(draws, 16)
drawmeans = apply(draws, 2, mean)
hist(drawmeans, breaks = seq(1, 5, by = 0.25), main = "500 means of 16 draws")

# Simulations using a Continuous Distribution

# 500 samples, sample size = 5, 15, 35, 50, X ~ Exp(lambda = 1.5)

par(mfrow = c(2,3))
for(n in c(5,15,35,50,200,1000)){
  draws <- rexp(n * 500, rate = 1.5)
  draws <- matrix(draws, n)
  drawmeans = apply(draws, 2, mean)
  hist(drawmeans, main = paste("500 means of ",n," draws", sep = ""))
}

# 500 samples, sample size = 5, 15, 35, 50, X ~ U([0,1])

par(mfrow = c(2,3))
for(n in c(5,15,35,50,200,1000)){
  draws <- runif(n * 200)
  draws <- matrix(draws, n)
  drawmeans = apply(draws, 2, mean)
  hist(drawmeans, main = paste("500 means of ",n," draws", sep = ""))
}

# SAMPLE DISTRIBUTION OF SAMPLE VARIANCE
# Simulations using a Continuous Distribution
# Assume that the underlying distribution X is distributed as X ∼ N(0, 9) and suppose that the sample size, n, is 6.

draws = matrix(rnorm(1000 * 6, 0, 3), 6)
drawvar = apply(draws, 2, var)
draws = 5 * drawvar/9
hist(draws, breaks = 20, prob = TRUE, main = "standard distribution for sample variance")
v = seq(0, max(draws), length = 200)
lines(v, dchisq(v, 5), lty = 2, lwd = 2)

# Sampling distribution of Proportion 
n <- 40
p <- 0.3
X <- rbinom(1000, n, prob = p)

Phat <- X/n
hist(Phat)












