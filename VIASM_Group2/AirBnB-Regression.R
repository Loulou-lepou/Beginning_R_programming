
## Importing packages

library(tidyverse)
library(lubridate)
library(ggcorrplot)
library(lattice)
library(psych)
library(DataExplorer)
library(reshape2)
library(car)
library(caret)
library(data.table)
library(e1071) 
library(scales)
library(stringr)
library(gridGraphics)
library(gridExtra)
library(cowplot)
library(lmtest)
library(gvlma)
library(mlbench)
options(warn=-1)
#install.packages("tableone")
library(tableone)
##################################################
# read data
setwd("/Users/macbookpro/PycharmProjects/RProject/VIASM_Group2")
df=read.csv("DA2-Airbnb-NYC-2019.csv")
head(df)
tail(df)
##################################################
# data description
# Id = listing ID
# name = name of the listing
# host_id = host ID
# host_name = name of the host
# neighbourhood_group = location
# room_type = listing space type
# price = price in dollars
# minimum_nights = amount of nights minimum
# number_of_reviews = number of reviews
# calculated_host_listings_count = amount of listing per host

# order the columns with target variable first
# name, host_id, & host_name can all be considered as id
df <- df %>% select(price, # place target variable first
                    id,
                    #name,                          
                    #host_id,
                    #host_name,
                    neighbourhood_group,
                    neighbourhood,                 
                    latitude, 
                    longitude,                     
                    room_type,                                               
                    minimum_nights, 
                    number_of_reviews,             
                    last_review, 
                    reviews_per_month,             
                    calculated_host_listings_count,
                    availability_365)
head(df, 4)
#####################################################
#-------------------------------------------#
###########    Missing values    ############
#-------------------------------------------#

###  --  Reviews per month  ---- ###

#  Replace NA's in the original dataframe with Zero's
df <- df %>% mutate(reviews_per_month = replace_na(reviews_per_month, 0))
#sum(is.na(df$reviews_per_month))

####  --------    last review    -------- ##

# to keep the actual date I'm going to make the data into number
df$last_review
df$last_review <- as.integer(gsub("-", "", df$last_review))
# set the missing values = 0
df <- df %>% mutate(last_review = replace_na(last_review, 0))
cat("Total Missing Values now = ",sum(is.na(df)) )
#--------------------------------------#
#######        Data Types        #######
#--------------------------------------#

columns <- c("neighbourhood_group","neighbourhood","room_type")
df[, columns] <- df %>% select(all_of(columns)) %>% lapply(as.factor)
# check to make sure it worked
df %>% select(all_of(columns)) %>% str()
#############################################
#-------------------------------------------#
#####   Visualize the Target Variable   #####
#-------------------------------------------#
#############################################
# try to predict price
options(repr.plot.width=10, repr.plot.height=6)
ggplot(df, aes(price)) + geom_histogram(col = "grey40", bins = 40) +
  labs(title = "NYC AirBnB Price Distribution", x = "Price", y = "Number of Rentals") +
  theme_bw(base_size = 18) + scale_x_continuous(labels = dollar)

options(repr.plot.width=10, repr.plot.height=3)
ggplot(df, aes(price)) + geom_boxplot(fill = "grey40") + 
  labs(title = "NYC AirBnB Price Distribution", x = "Price") +
  theme_bw(base_size = 18) + scale_x_continuous(labels = dollar)

# Price is severely skewed to the right wiht huge outliers
# filter to less than $1000
options(repr.plot.width=10, repr.plot.height=6)
df %>% filter(price < 1000) %>%
  ggplot(aes(price)) + geom_histogram(col = "grey40", bins = 40) +
  labs(title = "NYC AirBnB Price Distribution (Price less than $1,000)", 
       x = "Price", y = "Number of Rentals") +
  theme_bw(base_size = 18) + scale_x_continuous(labels = dollar)

options(repr.plot.width=10, repr.plot.height=3)
df %>% filter(price < 1000) %>%
  ggplot(aes(price)) + geom_boxplot(fill = "grey40") + 
  labs(title = "NYC AirBnB Price Distribution (Price less than $1,000)", x = "Price") +
  theme_bw(base_size = 18) + scale_x_continuous(labels = dollar)

#############################################
#-------------------------------------------#
######       Visualize the Data        ######
#-------------------------------------------#
#############################################

##### --- Price by Neighborhood Group  --- ######

options(repr.plot.width=10, repr.plot.height=8)
a = ggplot(df, aes(fct_reorder(neighbourhood_group, price), price)) + 
  geom_boxplot(fill = "grey30") + 
  labs(title = "Price Distribution by Neighborhood Group",
       x = "", y = "Price") + theme_bw(base_size = 18) +
  scale_y_continuous(labels = dollar)

b = df %>% filter(price < 1000) %>%
  ggplot(aes(fct_reorder(neighbourhood_group, price), price)) + 
  geom_boxplot(fill = "grey30") + 
  labs(title = "Price Distribution by Neighborhood Group (Price < $1,000)",
       x = "", y = "Price") + theme_bw(base_size = 18) +
  scale_y_continuous(labels = dollar)
# use gridExtra to combine all 4 plots
plot_grid(a,b, ncol = 1, nrow = 2)
##### --- Price by Room Type  --- ######

options(repr.plot.width=10, repr.plot.height=8)
a = ggplot(df, aes(fct_reorder(room_type, price), price)) + 
  geom_boxplot(fill = "grey30") + 
  labs(title = "Price Distribution by Room Type",
       x = "", y = "Price") + theme_bw(base_size = 18) +
  scale_y_continuous(labels = dollar)


b = df %>% filter(price < 1000) %>%
  ggplot(aes(fct_reorder(room_type, price), price)) + 
  geom_boxplot(fill = "grey30") + 
  labs(title = "Price Distribution by Room Type (Price < $1,000)",
       x = "", y = "Price") + theme_bw(base_size = 18) +
  scale_y_continuous(labels = dollar)
# use gridExtra to combine all 4 plots
plot_grid(a,b, ncol = 1, nrow = 2)
#####    Skew of the Distirbutions     ######

# does skewness deviate a lot from 1.0
cat("Skew of the Price Distribution = ", skewness(df$price)," > 1 is highly skewed towards the right")
a <- df %>% filter(price < 1000)
cat("\nSkew of the Price Distribution with outliers removed = ", skewness(a$price)," > 1 but much less skewed than before")
### ---   QQ-Plots ----- ###

options(repr.plot.width=14, repr.plot.height=6)
par(mfrow=c(1,2))
qqnorm(df$price);qqline(df$price)
qqnorm(a$price); qqline(a$price) # pretty normal
# Ho: data is normally distributed
s1 = sample_frac(df, size = .1, replace = F); shapiro.test(s1$price);
s2 = sample_frac(a, size = .1, replace = F); shapiro.test(s2$price) 

# p - value
cat("original dataset p-value = ", as.numeric(shapiro.test(s1$price)[2]), " < 0.05 = not normaly distributed")
cat("\noutliers removed dataset p-value = ", as.numeric(shapiro.test(s2$price)[2]), " < 0.05 = not normaly distributed but better")
#-----------------------------------------------#
#####   Identify & Remove outliers - Price  #####
#-----------------------------------------------#

Q <- quantile(df$price, probs=c(.25, .75), na.rm = T)
iqr <- IQR(df$price, na.rm = T)
df2 <- df %>% filter(price > (Q[1] - 1.5*iqr) & 
                       price < (Q[2] + 1.5*iqr))  

# visualize the new dataset without outliers
options(repr.plot.width=10, repr.plot.height=6)
par(mfrow=c(2,1))
boxplot(df$price, col = "grey40", horizontal = T, 
        main = "Price - Before Removing Outliers")
boxplot(df2$price, col = "thistle2", horizontal = T, 
        main = "Price - After Removing Outliers")
########################################
#--------------------------------------#
######         Correlations       ######
#--------------------------------------#
########################################

c1 <- df
#c1 %>% select_if(is.character) %>% names()
#cols = c("name", "host_name")
#c1[, cols] <- c1 %>% select(all_of(cols)) %>% lapply(as.factor)
#c1 %>% select_if(is.factor) %>% names()
cols = c("neighbourhood_group", "neighbourhood", "room_type")
c1[, cols] <- c1 %>% select(all_of(cols)) %>% lapply(as.numeric)

# data frame with outliers removed
c2 <- df2
#c2 %>% select_if(is.character) %>% names()
# cols = c("name", "host_name")
# c2[, cols] <- c2 %>% select(all_of(cols)) %>% lapply(as.factor)
#c2 %>% select_if(is.factor) %>% names()
cols = c("neighbourhood_group", "neighbourhood", "room_type")
c2[, cols] <- c2 %>% select(all_of(cols)) %>% lapply(as.numeric)
#----------------------------------------------
# create correlation matrix original data frame
#----------------------------------------------

cor <- cor(c1)
cols = c("price","id", # "name", "host_id","host_name",                                    
         "neighbourhood_group", "neighbourhood",                 
         "latitude", "longitude",                     
         "room_type",                         
         "minimum_nights", "number_of_reviews",             
         "last_review", "reviews_per_month",             
         "calculated_host_listings_count", "availability_365")

# stack the correlations
cor <- as_tibble(reshape2::melt(cor, id = cols))
# rename the columns appropriately
colnames(cor) <- c("Target", "Variable", "Correlation")

#------------------------------------------#
####    pick the target variables C1    ####
#------------------------------------------#

C <- cor[which(cor$Target == "price"),]
C <- C[order(- abs(C$Correlation)), ]
C <- subset(C, abs(C$Correlation) > 0.10); C
#------------------------------------------------------#
#### ----  Visualize the correlation matrix   ---- #####
#------------------------------------------------------#

options(repr.plot.width=12, repr.plot.height=12)
corr <- round(cor(c1, use="complete.obs"), 2)
ggcorrplot(corr, lab = TRUE, colors = c("aquamarine", "white", "dodgerblue"), 
           show.legend = F, outline.color = "gray", type = "upper", #hc.order = T,  
           tl.cex = 15, lab_size = 5, sig.level = .2,
           title = "Original Dataset Correlation Matrix") +
  labs(fill = "Correlation")

#-----------------------#
####  Pairs Panels   ####
#-----------------------#

options(repr.plot.width=10, repr.plot.height=8)
n1 <- c("price", "room_type", "longitude", "neighbourhood")
nc1 <- c1 %>% select(all_of(n1))
ncc1 <- cor(nc1)
pairs.panels(ncc1, hist.col = 'grey40', stars = T, cex.cor = .8)
#-----------------------------------------------------
# create correlation matrix data frame with no outliers
#-----------------------------------------------------

cor2 <- cor(c2)
# gather all the column names
#names(c1)
cols = c("price","id", # "name", "host_id","host_name",                       
         "neighbourhood_group", "neighbourhood",                 
         "latitude", "longitude",                     
         "room_type",                        
         "minimum_nights", "number_of_reviews",             
         "last_review", "reviews_per_month",             
         "calculated_host_listings_count", "availability_365")

# stack the correlations
cor2 <- as_tibble(reshape2::melt(cor2, id = cols))
# rename the columns appropriately
colnames(cor2) <- c("Target", "Variable", "Correlation")

#----------------------------------------#
####    pick the target variables C2  ####
#----------------------------------------#

C <- cor2[which(cor2$Target == "price"),]
C <- C[order(- abs(C$Correlation)), ]
C <- subset(C, abs(C$Correlation) > 0.10); C
corr <- round(cor(c2, use="complete.obs"), 2)
options(repr.plot.width=12, repr.plot.height=12)
ggcorrplot(corr, lab = TRUE, colors = c("aquamarine", "white", "dodgerblue"), 
           show.legend = F, outline.color = "gray", type = "upper", #hc.order = T,
           tl.cex = 15, lab_size = 5, sig.level = .2,
           title = "Dataset with Outliers Removed Correlation Matrix") +
  labs(fill = "Correlation")
#--------------------------------------#
######      Multicollinearity     ######
#--------------------------------------#

# model 1
m <- subset(c1, select = -c(price))
mc <- cor(m)
highlyCorrelated = findCorrelation(mc, cutoff=0.7)
highlyCorCol = colnames(m)[highlyCorrelated]
highlyCorCol # no Multicollinearity

#--------------------------------------------------#
#######      Check the Results using VIF      ######
#--------------------------------------------------#

# Choose a VIF cutoff under which a variable is retained 
# vif > 10  = multi-collinearity (Zuur et al. 2010)
#can also reject predictors with vf 5-10

# model 1
fit1 <- lm(price ~., data = c1); summary(fit1)
vif(fit1)

# model 2
m <- subset(c2, select = -c(price))
mc <- cor(m)
highlyCorrelated = findCorrelation(mc, cutoff=0.7)
highlyCorCol = colnames(m)[highlyCorrelated]
highlyCorCol # no Multicollinearity

# model 2
fit2 <- lm(price ~., data = c2); summary(fit2)
vif(fit2)
###################################################
#-------------------------------------------------#
###########      LINEAR REGRESSION    #############
#-------------------------------------------------#
###################################################

#------------------------------------------------------#
#####     Backward Elimination  (alpha = 0.05)     #####
#------------------------------------------------------#

# set an alpha level in advance and eliminate all variables
# that have a significance level above that alpha level

# remove the variable with the highest p-value then rerun
# the model, remove variable with highest p-value, until 
# only variable with significance above the alpha level remain

# sample the data
s1 = sample_frac(df, size = .3, replace = F)
s2 = sample_frac(df2, size = .3, replace = F)

# model 1 (including outliers)
m1 <- s1 %>% select(price, # place target variable first
                    #id,
                    #neighbourhood_group,
                    #neighbourhood,                 
                    latitude, 
                    longitude,                     
                    room_type,                                               
                    #minimum_nights, 
                    number_of_reviews,             
                    last_review, 
                    #reviews_per_month,             
                    #calculated_host_listings_count,
                    availability_365
) %>% lm()  
summary(m1)

# model 2 (excluding outliers)
m2 <- s2 %>% select(price, # place target variable first
                    id,
                    neighbourhood_group,
                    #neighbourhood,                 
                    latitude, 
                    longitude,                     
                    room_type,                                               
                    minimum_nights, 
                    number_of_reviews,             
                    last_review, 
                    #reviews_per_month,             
                    calculated_host_listings_count,
                    availability_365
) %>% lm()  
summary(m2)

cat("Is the adjusted R squared GREATER THAN the previous model? : ", as.numeric(summary(m1)[9]) < as.numeric(summary(m2)[9]),"\n");
cat('\nModel 1 Adjusted R Squared = ',as.numeric(summary(m1)[9])); 
cat('\nModel 2 Adjusted R Squared = ',as.numeric(summary(m2)[9]));

cat("Is the MSE LESS THAN the first model? ", mean(m1$residuals^2) > mean(m2$residuals^2), "\n");
cat("\nModel 1 MSE = ", mean(m1$residuals^2));
cat("\nModel 2 MSE = ", mean(m2$residuals^2))

#------------------------------------#
######    Assessing Outliers    ######
#------------------------------------#

# step 1) visualize the data for outliers

####    Histograms    #####

par(mfrow=c(2,2))
options(repr.plot.width=12, repr.plot.height=8)
hist(df2$minimum_nights, col = "grey50", main = "Minimum Nights") # right skewed
hist(df2$number_of_reviews, col = "grey50", main = "Number of Reviews") # right skewed
hist(df2$availability_365, col = "grey50", main = "Availability") # right skewed
hist(df2$calculated_host_listings_count, col = "grey50", main = "Listng Count") # right skewed

####      Boxplots     #####

par(mfrow=c(4,1))
options(repr.plot.width=10, repr.plot.height=8)
boxplot(df2$minimum_nights, col = "grey50", horizontal = T, main = "Minimum Nights")
boxplot(df2$number_of_reviews, col = "grey50", horizontal = T, main = "Number of Reviews")
boxplot(df2$availability_365, col = "grey50", horizontal = T, main = "Availability")
boxplot(df2$calculated_host_listings_count, col = "grey50", horizontal = T, main = "Calculated Host Listing Count")

par(mfrow=c(1,1)) # reset par

#------------------------------------------------------------#
#####    Identify & Remove outliers - minimum nights     #####
#------------------------------------------------------------#

Q <- quantile(df2$minimum_nights, probs=c(.25, .75), na.rm = T)
iqr <- IQR(df2$minimum_nights, na.rm = T)
df3 <- df2 %>% filter(minimum_nights > (Q[1] - 1.5*iqr) & 
                        minimum_nights < (Q[2] + 1.5*iqr))
# visualize the new dataset without outliers
options(repr.plot.width=10, repr.plot.height=6)
par(mfrow=c(2,1))
boxplot(df$minimum_nights, col = "grey40", horizontal = T, 
        main = "minimum nights - Before Removing Outliers")
boxplot(df3$minimum_nights, col = "thistle2", horizontal = T, 
        main = "minimum nights - After Removing Outliers")
#------------------------------------------------------------#
#####   Identify & Remove outliers - number_of_reviews   #####
#------------------------------------------------------------#

Q <- quantile(df3$number_of_reviews, probs=c(.25, .75), na.rm = T)
iqr <- IQR(df3$number_of_reviews, na.rm = T)
df3 <- df3 %>% filter(number_of_reviews > (Q[1] - 1.5*iqr) & 
                        number_of_reviews < (Q[2] + 1.5*iqr))
# visualize the new dataset without outliers
par(mfrow=c(2,1))
boxplot(df$number_of_reviews, col = "grey40", horizontal = T, 
        main = "number_of_reviews - Before Removing Outliers")
boxplot(df3$number_of_reviews, col = "thistle2", horizontal = T, 
        main = "number_of_reviews - After Removing Outliers")

#-------------------------------------------------------#
#######    Model with additional ouliers removed    #####
#-------------------------------------------------------#

# sample the data
s3 = sample_frac(df3, size = .4, replace = F)
# model 2 (excluding outliers)
m3 <- s3 %>% select(price, # place target variable first
                    #id,
                    neighbourhood_group,
                    #neighbourhood,                 
                    latitude, 
                    longitude,                     
                    room_type,                                               
                    minimum_nights, 
                    number_of_reviews,             
                    last_review, 
                    reviews_per_month,             
                    calculated_host_listings_count,
                    availability_365
) %>% lm()  
summary(m3)

#------------------------------------------------#
#####   Assessing the Accuracy of Models    ######
#------------------------------------------------#

cat("Model 2 Adjusted R-Squared = ", as.numeric(summary(m2)[9]),"\n");
cat("Model 3 Adjusted R-Squared = ", as.numeric(summary(m3)[9]),"\n");
cat("\nIs the adjusted R squared GREATER THAN the first model?", as.numeric(summary(m2)[9]) < as.numeric(summary(m3)[9]));
cat("\nIs MSE LESS THAN the previous model?", mean(m2$residuals^2) > mean(m3$residuals^2), "\n");
cat("\nModel 2 MSE = ", mean(m2$residuals^2));
cat("\nModel 3 MSE = ", mean(m3$residuals^2))

#------------------------------------------------------#
#####   Compare the Median - Non Parametric Data   #####
#------------------------------------------------------#

# check anova for F value significance
#anova(m1) # cannot use ANOVA for non-parametric data
kruskal.test(price ~ as.factor(room_type), data = df);
kruskal.test(price ~ as.factor(neighbourhood_group), data = df)
# p-value < 2.2e-16 < 0.05 = medians are different among regions

#-----------------------------------------#
####        Inetraction Terms        ######
#-----------------------------------------#

summary(lm(price ~ as.factor(room_type) * longitude, data = df2))

summary(lm(price ~ as.factor(room_type) * 
             calculated_host_listings_count, data = df2))

df4 <- df3
df4[,c("room_type", "neighbourhood", "neighbourhood_group", "calculated_host_listings_count")] <- df4 %>%
  select(room_type, neighbourhood, neighbourhood_group, calculated_host_listings_count) %>% lapply(as.factor)
df4 %>% str()

# sample the data
s4 = sample_frac(df4, size = .4, replace = F)
# model 2 (excluding outliers)
m4 <-lm(price ~ 
          #id + 
          neighbourhood_group + 
          #neighbourhood +                 
          latitude +  
          #longitude +                      
          room_type +                                               
          minimum_nights + 
          number_of_reviews +            
          last_review + 
          #reviews_per_month +             
          calculated_host_listings_count +
          availability_365 +
          (room_type * longitude) + 
          (room_type * calculated_host_listings_count),
        #(neighbourhood_group * longitude),
        #(neighbourhood_group * calculated_host_listings_count),
        data = s4); summary(m4)

#------------------------------------------------#
#####   Assessing the Accuracy of Models    ######
#------------------------------------------------#

cat("\nIs the adjusted R squared GREATER THAN the first model?", as.numeric(summary(m3)[9]) < as.numeric(summary(m4)[9]));
cat("\nModel 3 Adjusted R-Squared = ", as.numeric(summary(m3)[9]),"\n");
cat("Model 4 Adjusted R-Squared = ", as.numeric(summary(m4)[9]),"\n");
cat("\nIs MSE LESS THAN the previous model?", mean(m3$residuals^2) > mean(m4$residuals^2));
cat("\nModel 3 MSE = ", mean(m3$residuals^2));
cat("\nModel 4 MSE = ", mean(m4$residuals^2))

#---------------------------------------#
#########   data assumptions    #########
#---------------------------------------#

# 1) is there a linear relationship?
#   Yes - then a linear regression model is appropriate
#   No - a linear regression model is not appropriate

# 1) Normality - the residuals are normally distributed
#       QQplot of the residuals

# 3) Homoskedastic or Heteroskedastic?
#   is there a pattern to the reisduals?
#   Yes - there is a pattern, then the residuals are not random
#         and you should not proceed with this model = Heteroskedastic
#   No - there is no pattern, then the residuals are random
#         and it is okay to proceed with this model = Homoskedastic

# 4) Independent observations - is there a correlation between
#       the fit and residuals?
#     Yes - model does not meet independence
#     No - model does meet the independence test

#--------------------------------------#
#######    Diagnostic Plots     ########
#--------------------------------------#

options(repr.plot.width=14, repr.plot.height=8)
par(mfrow = c(2,2)); plot(m1, main = "Model 1")

par(mfrow = c(2,2)); plot(m2, main = "Model 2")

par(mfrow = c(2,2)); plot(m3, main = "Model 3")

par(mfrow = c(2,2)); plot(m4, main = "Model 4")

#--------------------------------------------------#
####      Autocorrelation between errors      ######
#--------------------------------------------------#

# Ho: There is no autocorrelation between errors
# meaning the errors are independent
# if p-value < 0.05 = Reject the Null Hyyppthesis 
cat("Model 1 p-value = ", as.numeric(dwtest(m1)[4])," = no autocorrelation between errors");
cat("\nModel 1 p-value = ", as.numeric(dwtest(m2)[4])," = no autocorrelation between errors");
cat("\nModel 1 p-value = ", as.numeric(dwtest(m3)[4])," = no autocorrelation between errors");
cat("\nModel 1 p-value = ", as.numeric(dwtest(m4)[4])," = no autocorrelation between errors")

# -------------------------------------------------#
########       Influential Observations     ########
# -------------------------------------------------#

#identify outliers that have too much influence on model
#influential data points

par(mfrow=c(2,2))

cutoff <- 4/((nrow(df)-length(m1$coefficients)-2)) 
plot(m1, which=4, cook.levels=cutoff, main = "Model 1") 

cutoff <- 4/((nrow(df)-length(m2$coefficients)-2)) 
plot(m2, which=4, cook.levels=cutoff, main = "Model 2") 

cutoff <- 4/((nrow(df)-length(m3$coefficients)-2)) 
plot(m3, which=4, cook.levels=cutoff, main = "Model 3") 

cutoff <- 4/((nrow(df)-length(m4$coefficients)-2)) 
plot(m4, which=4, cook.levels=cutoff, main = "Model 4")

# see a list of the outliers - note the p-value < 0.05
outlierTest(m1) # Bonferonni p-value for most extreme obs Model 1
outlierTest(m2) # Bonferonni p-value for most extreme obs Model 2
outlierTest(m3) # Bonferonni p-value for most extreme obs Model 3
outlierTest(m4) # Bonferonni p-value for most extreme obs Model 4

#------------------------------------------------#
#####   Assessing the Accuracy of Models    ######
#------------------------------------------------#

# adjusted R squared of all models
cat("Model 1 Adjusted R-Squared = ", as.numeric(summary(m1)[9]),"\n");
cat("Model 2 Adjusted R-Squared = ", as.numeric(summary(m2)[9]),"\n");
cat("Model 3 Adjusted R-Squared = ", as.numeric(summary(m3)[9]),"\n");
cat("Model 4 Adjusted R-Squared = ", as.numeric(summary(m4)[9]))

# SSE of all models
cat("Model 1 MSE = ", mean(m1$residuals^2),"\n");
cat("Model 2 MSE = ", mean(m2$residuals^2),"\n");
cat("Model 3 MSE = ", mean(m3$residuals^2),"\n");
cat("Model 4 MSE = ", mean(m4$residuals^2))

