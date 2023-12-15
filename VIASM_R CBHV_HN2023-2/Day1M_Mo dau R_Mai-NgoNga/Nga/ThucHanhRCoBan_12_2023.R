# 1. Gioi thieu R

# 2. Gioi thieu cua so RStudio

# 3. R hoat dong nhu the nao?
# Go lenh vao cua so R console
# Lenh la phep toan

# Lenh la mot ham: ten ham, cac tham so cach nhau boi sau phay

# Ham c(): nhap du lieu dang vector

# Ham data.frame(): nhap du lieu dang bang

# Tro giup trong R
# Doc help
?seq
help(seq)
# Tim kiem tren mang
# Xem cheatsheets
# Tim kiem ten ham chua cum ky tu bang apropos()
apropos(".test")

# 4. Dat ten doi tuong trong R
# Notes: bat dau boi chu cai, chi bao gom chu cai, chu so, dau (.), dau (_) 

# Phep gan trong R: = hoac <-
# Phim tat cho <-
  # Win: Alt + -
  # Mac: option + -

# 5. Cai dat them goi moi
# Cai goi foreign de doc file.sav
install.packages("foreign")
library(foreign)
# hoac Packages --> Install

# Goi goi moi de su dung
library(foreign)
# hoac
require(foreign)

# 6. Doc du lieu trong R 
# Buoc 1. Tao duong dan den thu muc can lay du lieu 

# Xem thu muc dang lam viec cua R
getwd() # get working directory

# Thiet lap thu muc lam viec moi
# Vi du thu muc de du lieu la: Documents -> R -> ThongKeXaHoi -> DuLieu
# setwd("Documents/R/ThongKeXaHoi/DuLieu/") # chu y la dau / (slash)
getwd() #kiem tra lai duong dan
setwd("F:/ThucHanhThongKeTrenR")
# Buoc 2. Doc du lieu tu cac phan mem khac nhau bang ham tuong ung ---

# Doc du lieu .csv bang ham read.csv() va dat ten DL1
DL1 <-  read.csv("CaSchool.csv", header = T)
# Xem du lieu
View(DL1)
# Xem mot so quan sat dau
head(DL1)

# Doc du lieu .rda/.Rdata  bang ham load() va dat ten DL2
DL2 = load("CaSchool.rda")

# Doc du lieu .sav  bang ham read.spss() va dat ten DL3

install.packages("foreign")
library(foreign)
DL3 <-  read.spss("BehaviorIntention.sav", to.data.frame = TRUE, use.value.labels = TRUE)
#hoac
DL3 <-  foreign::read.spss("BehaviorIntention.sav", to.data.frame = TRUE, use.value.labels = TRUE)
View(DL3)

# 7. Du lieu co san trong R

data() # liet ke du lieu co san trong goi co ban hoac goi goi ra trong R
data(package = .packages(all.available = TRUE)) # liet ke tat ca du lieu trong
                                                #nhung goi da cai trong R
View(cars) # xem du lieu cars
?cars # tim hieu du lieu cars

# 8. Truy cap du lieu dang bang (data.frame)
names(DL1) # xem ten bien
dim(DL1)   #dimention: xem so dong, so cot
str(DL1)   #structure: xem cau truc ben trong

# Truy cap quan sat, dong, cot. Chu y dung [], khong phai ()
# Truy cap bang so thu tu
DL1[1,4] # dong 1, cot 4
DL1[1,]  #dong 1
DL1[, 4] #cot 4

# Truy cap bang ten cot
DL1[c("county", "testscr")]

# Dung toan tu $
DL1$gr_span

# Loc du lieu bang cac bieu thuc tren ten cot

# Toan tu pipe

# ham select, filter


# 9. Tom tat du lieu
# 9.1 Tom tat du lieu bang bang tan so

# Ham table: tinh tan so
table(DL1$gr_span) #tan so cot gr_span
table(DL1$county, DL1$gr_span) # tan so cheo cot county va gr_span

# Ham prop.table: tinh tan suat
prop.table(table(DL1$gr_span))
round(prop.table(table(DL1$gr_span)), 2) #lam tron 2 chu so
prop.table(table(DL1$county, DL1$gr_span), margin = 1) #tan suat theo dong
prop.table(table(DL1$county, DL1$gr_span), margin = 2) #tan suat theo cot

# Ham cut: phan to du lieu
min(DL1$testscr)
max(DL1$testscr)
table(cut(DL1$testscr, 10))

# 9.2 Tom tat du lieu bang cac dai luong thong ke mo ta
mean(DL1$testscr) #tinh trung binh
median(DL1$testscr) #tinh trung vi
var(DL1$testscr) #tinh phuong sai
sd(DL1$testscr) #tinh do lech chuan
summary(DL1$testscr) #tinh min, max, tu phan vi

# 9.3 Tom tat du lieu bang bieu do
# Ham plot()
x <- rnorm(5)
y <- rnorm(5)
plot(x, y)
# hoac
plot(y ~ x)
title(main = "Bieu do tan xa", sub = "Du lieu ngau nhien")

plot(y ~ x,
    col = "red", # thay doi mau
    xlab = "Hoanh do", # them ten truc x
    ylab = "Tung do", # them ten truc y
    main = "Bieu do tan xa", # them tieu de chinh
    sub = "Du lieu ngau nhien" # them tieu de phu
    )
    


