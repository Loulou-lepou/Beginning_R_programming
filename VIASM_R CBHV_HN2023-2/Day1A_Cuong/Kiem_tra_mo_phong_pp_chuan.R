salaries <- read.csv("DA1-Professor-Salaries.csv", header = T)

xbar <- mean(salaries$salary)
s <- sd(salaries$salary)


# Kiểm tra pp chuẩn
x <- rnorm(50, mean = 20, sd = 0.75)
qqnorm(x)
qqline(x)

shapiro.test(x)

y <- rexp(50, rate = 1/3 )
qqnorm(y)
qqline(y)


# Kiểm tra pp chuẩn của salary

qqnorm(salaries$salary)
qqline(salaries$salary)

shapiro.test(salaries$salary)

# Khảo sát tỷ lệ học sinh đồng ý học online-offline

# Giả sử hỏi ý kiến n = 80 học sinh, có 70 học sinh đồng ý
alpha < 0.05
p.hat <- 70/80
n <- 80
z.score <- qnorm(alpha/2, lower.tail = F)

# Tinh sai số biên
E <- z.score*sqrt(p.hat*(1-p.hat)/n)

lower <- p.hat - E
upper <- p.hat + E

c(lower, upper)

binom.test(70, 80)

ci.prop <- function(x, n, alpha){
  
  z.score <- qnorm(alpha/2, lower.tail = F)
  p.hat <- x/n
  E <- z.score*sqrt(p.hat*(1-p.hat)/n)
  lower <- p.hat - E
  upper <- p.hat + E
  c(lower, upper)
}

# Kiểm định dùng hàm t.test
# Kiểm định 1 mẫu, H0: mu = 1200000, H1: mu < 120000

t.test(salary, mu = 120000, alternative = "less")

# Kiểm định so sánh hai mẫu độc lập
# Trích dữ liệu lương PGS/GS đại học thuộc nhóm A = "lĩnh vực lý thuyết" và B = "lĩnh vực ứng dụng"

X <- subset(salaries, discipline == "A") #
Y <- subset(salaries, discipline == "B")

# Lọc ra biến salary ở hai nhóm
x_salary <- X$salary 
y_salary <- Y$salary

# Kiểm tra xem phương sai bằng nhau hay khác nhau
var.test(x_salary, y_salary) 

# Kiểm đinh với giả sử phương sai bằng nhau, đối thuyết H1: mu_X < mu_Y
t.test(x_salary, y_salary, var.equal = TRUE, alternative = "less")

# Kiểm đinh với giả sử phương sai khác nhau, đối thuyết H1: mu_X # mu_Y
t.test(x_salary, y_salary, var.equal = FALSE)

# So sánh cặp
# Đọc dữ liệu file chế độ ăn kiêng
diet <- read.csv("Diet.csv", header = T)


t.test(diet$pre.weight, diet$weight6weeks, paired = TRUE, alternative = "greater")

