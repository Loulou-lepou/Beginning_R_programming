
# Mô phỏng một tổng thể về X = chiều cao thanh niên TP. HCM. Gs X ~ N(168, 5^2)

mu <- 168
sig <- 5

Pop <- rnorm(10^6, mu, sig)
hist(Pop)

# Chọn mẫu theo pp chọn mẫu ngẫu nhiên đơn giản

n <- 30 
S <- sample(Pop, n, replace = TRUE)
hist(S) 

# Kiểm tra pp chuẩn dùng đồ thị Q-Q norm
qqnorm(S)
qqline(S)

# Tìm khoảng tin cậy dùng hàm t.test

CI <- t.test(S, mu = 0, conf.level = 0.99)
CI$conf.int

# HỎI Ý KIẾN VỀ VIỆC HỌC ONLINE /OFFLINE CỦA HỌC SINH

# Giả sử tỷ lệ học sinh ủng hộ học trực tiếp là p = 0.8

p <- 0.8

# Hỏi ý kiến n học sinh : Y1, Y2, ... Yn, 
# Y = sum(Y_i) => Y ~ B(n, p)

# Vẽ phân phối của Y khi n nhỏ
n <- 10
m <- 1000
Y <- rbinom(m, n, p)
hist(Y)


# Khi n lớn, theo định lý giới hạn trung tâm Y xấp xỉ phân phối chuẩn

n <- 40
m <- 1000
Y <- rbinom(m, n, p)
hist(Y)








