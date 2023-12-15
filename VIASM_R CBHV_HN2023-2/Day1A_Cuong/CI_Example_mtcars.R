
data("mtcars")

xbar <- mean(mtcars$mpg)
print(xbar)

n  <- length(mtcars$mpg)
s  <- sd(mtcars$mpg)
SE <- s/sqrt(n) #S.E = Standard Error 
print(SE)

alpha = 0.05
df = n - 1
t.score = qt(alpha/2, df, lower.tail = F)
print(t.score)

E <- t.score * SE
lower <- xbar - E
upper <- xbar + E

print(c(lower,upper))

# Use t.test function (faster way)

ktc <- t.test(mtcars$mpg, conf.level = 0.99)
ktc$conf.int

# Use lm(.) function (faster way)

# Calculate the mean and standard error
l.model <- lm(mpg ~ 1, data = mtcars)

# Calculate the confidence interval
confint(l.model, level = 0.95)


