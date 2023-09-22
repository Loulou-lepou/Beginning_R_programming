# df <- read.csv("C:\\Users\\Admin\\Desktop\\bmi\\bmi.csv")
df <- read.csv("https://raw.githubusercontent.com/Loulou-lepou/Beginning_R_programming/main/bmi.csv")
# View(df)
bmi.index <- df$Weight / (df$Height * 0.01) ^ 2
diag <- df$index 
diag[bmi.index <= 16] <- "Severe Thinness"
diag[bmi.index > 16 & bmi.index <= 17] <- "Moderate Thinness"
diag[bmi.index > 17 & bmi.index <= 18.5] <- "Mild Thinness"
diag[bmi.index > 18.5 & bmi.index <= 25] <- "Normal"
diag[bmi.index > 25 & bmi.index <= 30] <- "Overweight"
diag[bmi.index > 30 & bmi.index <= 35] <- "Obese Class I"
diag[bmi.index > 35 & bmi.index <= 40] <- "Obese Class II"
diag[bmi.index > 40] <- "Obese Class III"
bmi.index <- round(bmi.index, digits=2)
new.df <- cbind(df, bmi.index, diag)
#View(new.df)
plot.new()
female.weight <- df$Weight[df$Gender=="Female"]
hist(female.weight, 
     breaks=100, main="Weights of females", xlab="weight(kg)")

plot.new()
female.height <- df$Height[df$Gender=="Female"]
hist(female.height, 
     breaks=100, main="Heights of females", xlab="height(kg)")

boxplot(female.height,
        main="Heights of females", xlab="height(kg)")

plot(female.height, female.weight, 
     pch=1, col="dark green", cex=0.5)
