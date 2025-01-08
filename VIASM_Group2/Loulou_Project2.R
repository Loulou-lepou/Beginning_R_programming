# Goals:
# price predictions 

# load modules
# use DT::datatable to create interactive data tables to easily view multi-column data.frame
library(DT)   
# use dplyr::glimpse to see overview of data frame's data structures

library(dplyr)
# naniar (Japanese) = what's missing -> handling missing data
library(naniar)
# create data visualizations
library(ggplot2)

# --------------------------------------------------------------------------------------
# import data & create a data frame object data
setwd("/Users/macbookpro/PycharmProjects/RProject/VIASM_Group2")

airbnb.data <- read.csv("DA2-Airbnb-NYC-2019.csv", header=T)
# 1st col: identifier, not a var, extract data from col 2 to the end
airbnb.data <- airbnb.data[ , 2:length(airbnb.data)]

# use only 1 data file -> attach the file to avoid typing prefix data.frame
# data.frame$column.name -> column.name 
# attach(airbnb.data)    

# View(airbnb.data)    
# head(airbnb.data, 5) -> many columns -> the first 5 rows wasn't displayed nicely.
# DT::datatable -> display interactive (dynamic, paginated, searchable) data tables

# datatable(airbnb.data, 
#           options=list(
#                         pageLength=5,        # show 5 first rows = 5 entries
#                         searching=T, 
#                         order=list(list(1,"asc"))    # sort by 1st col in ascending order 
#                       )
#           )

# interactively show the first 5 rows of airbnb.data
# class = 'cell-border stripe' -> borders around each cell
# -> applies a striped background to alternate rows
datatable(head(airbnb.data, 5), class = 'cell-border stripe')

# display dimension of the given data frame
# dim(airbnb.data)
print(paste("The data consists of", nrow(airbnb.data), "rows,", ncol(airbnb.data), "columns"))

# display data types: -> can see in Environment tab
column.names <- names(airbnb.data)
num.col <- ncol(airbnb.data)
# for (i in 1:num.col){
#   print(paste(column.names[i], ":", class(airbnb.data[, i]),":", airbnb.data[1, 1],";", airbnb.data[2, 1]))
# }

# dplyr::glimpse(airbnb_data) -> concise, informative overview of the data frame's structure 
# col_name : data_type: few example values 
# hidden info, can't be seen in the interactive data table
glimpse(airbnb.data)

# --------------------------------------------------------------------------------------

# Data preparation: 
# Prep 1. - avoid misidentifying data types for vars 
# Prep 2. - missing vals
# 

# Prep 1. converting to correct data types

# convert room type, neighbourhood_group to a categorical variable 
# is.factor(room_type)     # F
airbnb.data$room_type <- as.factor(airbnb.data$room_type)
print(paste("Room type:", unique(airbnb.data$room_type)))
airbnb.data$neighbourhood_group <- as.factor(airbnb.data$neighbourhood_group)

# data$host_id : vector 
is.vector(airbnb.data$host_id)
# unique() funct -> remove duplicates 
print(paste("There are", length(unique(airbnb.data$host_id)), "hosts"))

# Prep 2. Handling missing data

# count number of missing values of the whole data frame:
print(paste(sum(is.na.data.frame(airbnb.data)), "missing values"))

# iterate through columns to see missing values in columns
na.sum.vector <- c()
for (col.id in 1: num.col){
  num.na <- sum(is.na(airbnb.data[ , col.id]))
  if (1 <= num.na) {
    print(paste("Column:", column.names[col.id],"contains", num.na, "missing value(s)")) 
    na.sum.vector <- c(na.sum.vector, c(col.id, num.na))       
  }
}
na.sum.vector

# Visualization of missing data with naniar::gg_miss_var()
# "tidyverse style guide" => put + (concatenating sign) at the end of line
# + => add another layer / modify existing plot(s)
# set the theme to a minimalistic style. 
# 
dev.off()
dev.new()     
gg_miss_var(airbnb.data) + 
  theme_minimal() +     # gg = grammar of graphics
  labs(y = "\n Number of missing Values", title = "Missing values") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0))

# 
# --------------------------------------------------------------------------------------
# Descriptive Statistics 
# Desc 1. Summary of the data frame
summary(airbnb.data)
print(paste("The average price per night = $", round(mean(airbnb.data$price), 3)))

# Desc 2. Room type (CAT.) -> pmf, pie chart, bar chart 
# all room types:
print(paste("Room Type:", unique(airbnb.data$room_type))) 
# table() function -> create a frequency table for the cat. var: room_type  
room.type.table <- table(airbnb.data$room_type)
room.type.table

# bar chart for room_type using ggplot2::ggplot, ggplot2::geom_bar, ggplot2::geom_text
# convert table to data frame then use the data frame to plot bar graph
# room.type.dataframe consists of 2 columns: room_type, Freq

room.type.dataframe <- as.data.frame(room.type.table)

ggplot(room.type.dataframe, 
       mapping = aes(x=names(room.type.table), y = Freq)) + 
  geom_bar(stat="identity",    # y values represent the heights of the bars directly
           fill="skyblue", 
           color="black",
           width = 0.7)+
  labs(title = "Bar plot of room type", 
       x = names(room.type.table), 
       y = "Frequency") + 
  geom_text(aes(label=Freq),
            position=position_dodge(width=0.7),
            vjust=-0.5,
            color="black", 
            size = 3)+
  theme_minimal()

# pie chart for room_type.
room.type.percentages <- round(room.type.table / sum(room.type.table) * 100, 2)
pie(room.type.table, 
    labels=paste(names(room.type.table), "\n", room.type.percentages, "%"),
    col=rainbow(length(room.type.table)),
    radius = 1, angle=90,
    main="Room Type")

# display pie chart and bar chart in 1 figure
par(mfrow=c(1, 2))
barplot(room.type.table)
pie(room.type.table, 
    labels=paste(names(room.type.table), "\n", room.type.percentages, "%"),
    col=rainbow(length(room.type.table)),
    radius = 1, angle=90,
    main="Room Type", cex = 0.8)

# drop the shared room type as its count is very low compare to others
airbnb.2 <- airbnb.data[airbnb.data$room_type != "Shared room", ]

airbnb.2 <- airbnb.2[ , c(4, 6,7, 8, 9, 10, 11, 13, 14, 15)]
datatable(head(airbnb.2, 5), class = 'cell-border stripe')

# irrelevant vars: name, host_id, host_name 
# Desc 3. neighbourhood_group (CAT) 
# see how popular each neighbourhood_group wrt airbnb apartment number
neighbourhood_group.table <- table(airbnb.2$neighbourhood_group)
par(mfrow=c(1, 1)) # reset to a single-plot layout
pie(neighbourhood_group.table,
    labels= paste(names(neighbourhood_group.table), round(neighbourhood_group.table/ sum(neighbourhood_group.table) * 100), "%"),
    col = rainbow(length(neighbourhood_group.table)),
    radius=1, main="Neighbourhood group", cex=0.8)
# 

# Desc . price 
# - drop outliers 
# 
summary(airbnb.2$price)
# drop outliers from price 
# lower.bound <- quantile(price, .25)  - 1.5 * IQR(price)
# upper.bound <- quantile(price, .75) - 1.5 * IQR(price)
# airbnb.data.2 <- reduced.airbnb.data %>% filter(lower.bound <= price, price <= upper.bound)
# print(paste("After removing outliers, the average price would be $", mean(airbnb.data.2$price)))

# boxplot(price): not good -> some price is far too high in comparison with others

# 
# --------------------------------------------------------------------------------------
# selected.vars <- as.vector(column.names[c(4, 6,7, 8, 9, 10, 11, 13, 14, 15)])
# selected.vars contains non-numeric vars -> decode those vars
airbnb.2$neighbourhood_group <- as.numeric(airbnb.2$neighbourhood_group)
# airbnb.2$room_type <- as.numeric(airbnb.2$room_type)

pairs(airbnb.2)
# 

airbnb.model <- lm(airbnb.2$price ~ airbnb.2$neighbourhood_group +
                     airbnb.2$latitude + airbnb.2$longitude +
                     airbnb.2$minimum_nights + airbnb.2$room_type+
                     airbnb.2$number_of_reviews + airbnb.2$reviews_per_month+
                     airbnb.2$calculated_host_listings_count +
                     airbnb.2$availability_365)

summary.airbnb.model <- summary(airbnb.model)
summary.airbnb.model
