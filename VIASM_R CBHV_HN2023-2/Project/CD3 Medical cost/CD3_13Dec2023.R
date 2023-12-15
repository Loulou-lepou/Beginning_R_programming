library(tidyverse)
library(gtsummary)
library(gt)
setwd("~/1_Phan mem R")
DL1 <-  read.csv("insurance.csv", header = T)
names(DL1)
summary(DL1)
DL1 %>% 
  ggplot(aes(x=smoker, y = charges, fill = smoker, colour = smoker)) + 
  geom_violin()
#1.Thống kê mô tả các biến và phụ thuộc lẫn nhau (gói gtsumary) (Hạnh, Thu, Trang, Lê Phương)
DL1 %>% 
  ggplot(aes(x= bmi, y= charges, colour = smoker)) + geom_point()
#2. Kiểm định
#2.1. Kiểm định tham số (trung bình, trung vị, phương sai, tỉ lệ) theo các biến khác nhau (Tiến Thịnh, Hoàng Việt)
#2.2. Kiểm định phân phối (Danh Tiến, )
#2.3. Kiểm định tính độc lập của 2 biến định tính ()
#Biến charges có theo phân phối chuẩn 
#chia khoảng biến charges và tuổi...
#3. hồi quy (Việt Anh, Hoàng Lan, Trần Lâm, Nhung, Bùi Phương)
tbl_summary(DL1, 
            
            statistic = list(all_continuous() ~ "{mean} ({sd})"))
Tab1word <- tbl_summary(DL1, 
                        statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label  ~ "**Variables**") %>% 
  as_gt()
Tab1word |> gtsave("Tab1word.docx")
#gt::gtsave(data = Tab1word,  filename = "Tab1word.docx")
 
tab1 <- tbl_summary(DL1, 
            by = sex,
            statistic = list(all_continuous() ~ "{mean} ({sd})"))
tab1N <- add_p(tab1, test = list(all_continuous() ~ "t.test",
                                 all_categorical() ~ "chisq.test"))  %>%
  modify_header(label  ~ "**Biến quan sát**")%>% 
  as_gt()
gt::gtsave(data = tab1N,  filename = "tab1N.docx")