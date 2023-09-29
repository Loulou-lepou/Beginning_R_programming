hits <- c(0:5)
regions <- c(229, 211, 93, 35, 7, 1)
bombs.data <- data.frame(hits, regions)

# num.bombs <- 0
# for (i in c(1:6))
#   {num.bombs <- num.bombs + hits[i] * regions[i]}

num.bombs <- sum(hits * regions)
lambda <- num.bombs / 576

# print(paste("# bombs = ", num.bombs))
# print(paste(" average # bombs / region", lambda))

poisson.pmf <- dpois(hits, lambda)
# poisson.pmf <- c()
# for (i in c(1:6)){
#     poisson.pmf[i] <- dpois(hits[i], lambda)
# }
theoretical.data <- round(576 * poisson.pmf, 2)

# bombs.data <- data.frame(hits, regions, theoretical.data)
# View(bombs.data)

result <- cbind(bombs.data, theoretical.data)
# View(result)

plot.new()

# divide the figure into subplots (1 row & 2 columns) 
# mfrow = multi-frame ROW-wise layout
# par(mfrow=c(1,2))      

plot(hits, regions, pch=19, main="Rockets and Targets")

# plot(hits, theoretical.data, pch=23)
# do not use plot() for subsequent plots 
# which will overwrite the previous plots

points(hits, theoretical.data, pch="*", col="blue")

legend(2.5, 200, 
       legend=c("actual data", "theoretical data"),
       pch=c("o", "*"),
       ncol=1)

 
# compute the distance between 2 vectors
# sum.1 <- 0
# for (i in c(1:6)){sum.1 <- sum.1 + (theoretical.data[i] - regions[i])^2}
# distance.1 <- sqrt(sum.1)
# print(distance.1)

print(sqrt(sum((theoretical.data - regions)^2)))

