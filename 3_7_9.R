# 9.  Use of multiple linear regression on the Auto data set.

# manually changed the slashes because there isn't a good way to do it automatically.
my_filepath = 'C:/Users/Austin/Documents/ISLR_Labs'
setwd(my_filepath)
Auto=read.table("Auto.data", header = TRUE)
Auto$horsepower <- as.numeric(Auto$horsepower)

# a) Produce a scatterplot matrix which includes all of the variables in the dataset
plot(Auto)

# b) Compute the matrix of correlations between the variables using the function cor().  You will need to
#   to exclude the name variable, which is qualitative.
cor(subset(Auto, select = -c(name)))

# c) Use the lm() function to perform a multiple linear regression with mpg as the response
#   and all other variables except name as the predictors.  Use the summary() function to print
#   the results.  
Auto_less_name = subset(Auto, select = -c(name))
mlr_auto = lm(mpg ~ ., data = Auto_less_name)
summary(mlr_auto)

##    Comment on the output.  For instance:
## i.  Is there a relationship between the predictors and the response?
#####    Yes, the overall F statistic is quite robust, and the R^2 is around .82.

## ii. Which predictors appear to have a statistically significant relationship?
#####    displacement, weight, acceleration, year, and origin all have statistical significance.

## iii.What does the coefficient for the year variable suggest?
#####    The coefficient for the year suggests that new cars get more efficient over time each year.

# d) Use the plot() function to produce diagnostic plots of the linear regression fit.  
#    Comment on any problems you see with the fit.  Do the residual plots suggest any unusually large outliers?
# Does the leverage plot identify any observations with unusually high leverage?

plot(mlr_auto)
  # There are a lot of unusually high outliers to the top right of the chart.
  # There is a discernible curve pattern to the residuals which suggests an inaccurate fit.
  # Point 14 has high leverage.

# e) Use * and : to fit linear regression models with interaction effects.
mlr_auto2 = lm(mpg ~ cylinders*displacement+displacement*weight, data = Auto_less_name)
summary(mlr_auto2)
##### Displacement x Weight comes out significant.
summary(lm(mpg ~ cylinders + cylinders:displacement+ displacement:weight + displacement + weight, data = Auto_less_name))

# f) Try a few different transformations of the variables such as log(x), sqrt(x), x^2.  Comment on findings.
summary(lm(mpg ~ cylinders*displacement+displacement*weight + sqrt(weight), data = Auto_less_name))
### This particular transformation, sqrt(weight), didn't do too much.