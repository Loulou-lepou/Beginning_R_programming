#Nghien cuu mo phong
# B1: Sinh mau ngau nhien
n<- 100 #co mau
mu<- 45 # trung binh
sigma<- 7 # do lech chuan
set.seed(1)
x<- rnorm(n, mean=mu, sd=sigma)
x
head(x)
tail(x)
head(x, 9)
# B2: Tinh toan
x.bar<- sum(x)/n # uoc luong diem cho mu
s2<- sum((x-x.bar)^2)/n # uoc luong chech cua sigma^2
s2.star<- s2*n/(n-1) # uoc luong khong chech cua sigma^2
# B3: Uoc luong khong chech
mu.hat<- x.bar
sigma.hat<- sqrt(s2.star)
# B2: Khoang tin cay khi biet sigma
alpha<- 0.05
z.alpha2<- qnorm(p=1-alpha/2) 
# phan vi muc alpha/2 cua phan phoi chuan tac
e1<- z.alpha2*sigma/sqrt(n) # sai so bien
l1<- x.bar- e1
u1<- x.bar + e1
c(l1, u1) # khoang tin cay 95% cua mu
##################################
# B3: Khoang tin cay khi chua biet sigma
alpha<- 0.05
t.alpha2<- qt(p=1-alpha/2, df=n-1)
# phan vi muc alpha/2 cua phan phoi Student t voi n-1 bac tu do
e2<- t.alpha2*sigma.hat/sqrt(n) # sai so bien
l2<- x.bar- e2
u2<- x.bar + e2
c(l2, u2) # khoang tin cay 95% cua mu
#################################
# Dung cac lech co san trong R
install.packages("BSDA")
require(BSDA)
z.test(x, alternative = "two.sided", mu=45, sigma.x = sigma, conf.level = 1-alpha)
t.test(x, alternative = "two.sided", mu=45, conf.level = 1-alpha)
