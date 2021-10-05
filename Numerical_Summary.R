
#---------------------------------------------------------
#                      DATA TYPES OF VARS
#---------------------------------------------------------
# table(airquality$Month)  # -> categorical var 


#---------------------------------------------------------
#                     CLEANING UP/ PREPROCESSING
#---------------------------------------------------------

# --------------------1. NAs------------------------------
- NAs (not available, missing observation)
  # summary(airquality$Ozone, na.rm = T)

# --------------------2. outliers -------------------------
- outliers 
  # boxplot(count~spray, 
  #           data = InsectSprays, 
  #           col = 'lightgray')
  # c.1 <- InsectSprays[InsectSprays$spray == 'C',]
  # q <- quantile(c.1[,1], c(0.25, 0.75))
  # iqr <-IQR(c.1[,1])
  # lb <- q[1]-1.5*iqr
  # ub <- q[2] + 1.5 * iqr
  # hist(c.1[,1])
  # plot(c.1[,1])
  # boxplot(c.1[,1], horizontal=T)


#---------------------------------------------------------
#                      DESCRIPTIVE STATISTICS
#---------------------------------------------------------

# --------------------1. Slicing & dicing-----------------

# df[df$Height>=77 & df$Weight>=242, ]
   - slicing  = filter rows 
   - dicing = select cols : [], $, df['colName']

   - data frame dimensions: nrow, ncol, dim 

# -------------------2. Numerical summary-----------------
 
  - 5 - number summary: min, Q1, Q2, Q3, max => summary()
  - min, max
  - mean, median, mode (central tendency)
  - var, sd (dispersion about the mean)
  - quantiles: quantile, IQR(interquartile range) = Q3-Q1

  - table -> count & cross tabulation [categorical vars]
  - apply function:   
    + apply(data_frame, 1(row)/ 2(col), func, na.rm = T)
    + # apply(iris[,1:4],1,mean)
      # apply(airquality,2,mean, na.rm=T)
            
    
# -------------------3.Graphical summary-----------------          


