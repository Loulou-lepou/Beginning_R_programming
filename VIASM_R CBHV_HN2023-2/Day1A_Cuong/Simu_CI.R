library(PASWR2)

m = 20
s = 3

cisim(samples = 100, n = 50, parameter = m, sigma = s, conf.level = 0.95)

#cisim(100, 50, 0.5, type = "Pi", conf.level = 0.95)

# Mô phỏng phân phối mẫu của trung bình

# X = chiều cao thanh niên 1 thành phố, X ~ N(168, 5^2)

mu <- 168; sig <- 5 

#pop <- rnorm(10^6, mu, sig)
pop <- runif(10^6, 165, 170) # uniform([a = 165, b = 170])
#pop <- rexp(10^6, 1/3)
hist(pop)

# Chọn mẫu cỡ n = 20 và tính trung bình 
n <- 20
S <- sample(pop, n, replace = T)
mean(S)

# Lặp lại việc chọn mẫu m = 500 lần 
m <- 500
S <- matrix(0, nrow = n, ncol = m)
for(i in 1:m){
  S[,i] <- sample(pop, n)
}

# Tính các trung bình mẫu
xbar <- apply(S, 2, mean)
xbar

hist(xbar)

