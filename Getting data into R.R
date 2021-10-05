#---------------------------------------------------------
#                      GETTING DATA
#---------------------------------------------------------
Data frame:
  rows : observations
  cols : features/ attributes/property
  
# --------------------1. MANUALLY-------------------------
vector :, c(), seq, rep 
data.frame

# --------------------2. read.csv()-----------------------
2. read.csv(), View(), rm()

file types: csv, txt

locations: 
  - computer : file.choose(), attach() & detach()
  # df <- read.csv(file.choose())
  # data.1 <- read.csv("/Users/macbookpro/PycharmProjects/RProject/weight-height.csv")
  
  - internet : URL # UCI, Kaggle, github
  # df <- read.csv('https://raw.githubusercontent.com/Loulou-lepou/LouBeginningPython/master/weight-height.csv')
  
  - R library : 
         + datasets  
         # airquality, ToothGrowth, InsectSprays, cars, iris
         
         + ggplot2 (inside 'tidyverse')  # -> Hadley Wickham, Garrett Grolemund
         # mpg