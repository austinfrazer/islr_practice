# 8.  Use of simple linear regression on the Auto data set.

# manually changed the slashes because there isn't a good way to do it automatically.
my_filepath = 'C:/Users/Austin/Documents/ISLR_Labs'
setwd(my_filepath)
Auto=read.table("Auto.data", header = TRUE)
Auto$horsepower <- as.numeric(Auto$horsepower)

# a) Use lm() to do simple linear regression with mpg as the response and horsepower as the predictor.
my_lm = lm(mpg ~ horsepower, data = Auto)
my_lm_summary = summary(my_lm)
my_lm_summary

### i)  Is there a relationship between the predictor and the response?
    # Yes.  It is significant.  But it is a bit counterintuitive  It says as horsepower goes up by 1,
    #     mpg goes up by .11.  I would have expected a negative relationship

### ii)  How strong is the relationship?
      # Very strong

### iii)  Postive or negative?
# Positive.

### iv) What is the predicted mpg associated with a horsepower of 98?  What are the associated confidence and prediction intervals?
my_lm_hp_98 = data.frame(horsepower=98)
predict(my_lm, my_lm_hp_98, interval="confidence")
predict(my_lm, my_lm_hp_98, interval="predict")


# b) Plot the response and the predictor.  Use the abline() function to display the least squares line.
plot(Auto$mpg, Auto$horsepower)
abline(my_lm)

# c) Use the plot() function to produce diagnostic plots of the least squares regression fit.
#       Comment on any problems you see with the fit.
plot(my_lm)

####### The first plot is a residuals vs fitted chart.
## Per https://online.stat.psu.edu/stat462/node/117/, we should expect these
##  characteristics if the plot is one of a well-behaved model:

# The residuals "bounce randomly" around the 0 line.
# This suggests that the assumption that the relationship is linear is reasonable.
####  OUR PLOT DOES NOT BOUNCE AROUND ON BOTH SIDES OF THE 0 line.

# The residuals roughly form a "horizontal band" around the 0 line.
# This suggests that the variances of the error terms are equal.
####  OUR PLOT HAS TWO DOWNWARD (NW-SE) BANDS.

# No one residual "stands out" from the basic random pattern of residuals.
# This suggests that there are no outliers.
#### SEVERAL OUTLIERS EXIST AND DEFINITELY STAND OUT.  331, 326, AND 323 EVEN HAVE OUTLIER LABELS.

####### The next plot is a QQ plot of the standardized residuals.
## Per https://data.princeton.edu/wws509/notes/c2s9, we know:
## In general, Q-Q plots showing curvature indicate skew distributions, 
## with downward concavity corresponding to negative skewness (long tail to the left)
## and upward concavity indicating positive skewness. 
## On the other hand, S-shaped Q-Q plots indicate heavy tails, or an excess of extreme values, 
## relative to the normal distribution.
#### OUR PLOT IS MOSTLY AN UPWARD CONCAVE FUNCTION, BUT IT HAS A LITTLE BIT OF AN S SHAPE AT THE VERY TOP.
####  I THINK THAT INDICATES THAT THERE IS MOSTLY POSTIVE SKEWNESS, BUT THAT THERE IS A FAIR AMOUNT
####  OF GENERAL EXTREMENESS.

###### The next plot is a scale-location plot.
## Per https://data.library.virginia.edu/diagnostic-plots/, we know:
## This plot shows if residuals have non-linear patterns. 
## There could be a non-linear relationship between predictor variables
## and an outcome variable and the pattern could show up in this plot if
## the model doesn’t capture the non-linear relationship. 
## If you find equally spread residuals around a horizontal line without distinct patterns, 
## that is a good indication you don’t have non-linear relationships.
#### OUR PLOT IS DEFINITELY HUMP-SHAMPED; THEREFORE, WE HAVE NON-LINEAR RELATIONSHIPS IN THE DATA.

###### The final plot is a residuals vs leverage plot.
##  Per Bommae Kim's article https://data.library.virginia.edu/diagnostic-plots/, we know:
##   Unlike the other plots, this time patterns are not relevant.
##   We watch out for outlying values at the upper right corner or at the lower right corner.
##   Those spots are the places where cases can be influential against a regression line.
##   Look for cases outside of a dashed line, Cook’s distance. When cases are outside of the Cook’s distance
##   (meaning they have high Cook’s distance scores), the cases are influential to the regression results.
##   The regression results will be altered if we exclude those cases.
#### OUR RESIDUALS V LEVERAGE PLOT SEEMS FINE.  WE DON'T EVEN SEE THE COOK'S DISTANCE LINE HERE.