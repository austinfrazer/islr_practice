# Chapter 2, Problem 10.

# Use Boston data.
# a) load data
library(MASS)

Boston
?Boston

# My own exploration of the "black" variable.  Very unintuitively named and transformed.
#  Untransformed it to what is termed Bk in the dataset.  Bk = proportion of blacks in a town.
#  I named this grey.  Row{Min(grey)} = Row{Max(black)}.

range(Boston$black)
grey = Boston$black
grey = grey/1000
range(grey)
grey = sqrt(grey)
range(grey)
grey = -grey + 0.63
range(grey)

Boston2 = data.frame(Boston, grey)  # Tacking on my preferred race variable on the Boston data set.

# How many rows in this data set?
number_of_rows = nrow(Boston)

# How many columns in this data set?
number_of_columns = ncol(Boston)

# What do the rows and columns represent?
print(paste("There are", number_of_rows, "observations and", number_of_columns, 
            "variables in the Boston data set."))

# b) Make some pairwise scatterplots of the predictors.  Describe your findings.
attach(Boston2)
detach(Boston2)
plot(Boston2)
plot(grey, black)
plot(grey, medv)
 # There are a lot of non-linear associations, and there are many outliers.
   # The closest to linear relationship that I see between a regressor and regressand are
      # between rooms and median value (medv ~ rm)
      # between proportion of the population of lower status and median value (medv ~ lstat)

# c) Are any of the predictor associated with the per capita crime rate (crim)?
crim_cor = apply(X = Boston2, MARGIN = 2, FUN = cor, x = crim)
sort(crim_cor)
  # The correlations greater than abs(.3) are:
    # medv, -.388; black, -.385; dis, -.380; age, .353; grey, .386; indus, .407;
    # nox, .421; lstat, .456; tax, .583; rad, .623

# d) Do any of the suburbs of Boston appear to have particularly
# high crime rates? Tax rates? Pupil-teacher ratios? Comment on
# the range of each predictor

    # crim
range(crim)  # This range is very, very broad
range(crim)[2]/range(crim)[1]  # 14078.51x spread
Boston[ which(Boston$crim==max(Boston$crim)), ]  # Here is the highest value (obs 381)

    # tax
range(tax)  # This range is fairly broad
range(tax)[2]/range(tax)[1]  # 3.80x spread.
sort(tax)
Boston[ which(Boston$tax==max(Boston$tax)), ]  # Here are the highest values (obs 489 - 493)

    # pupil teacher ratio
range(ptratio)  # 12.6 - 22.0
range(ptratio)[2]/range(ptratio)[1]  # 1.75x spread.
sort(ptratio)
Boston[ which(Boston$ptratio==max(Boston$ptratio)), ]  # Here are the highest values (obs 355 - 356)

# e) How many of the suburbs in this data set bound the Charles river?
Boston[ which(Boston$chas == max(Boston$chas)), ]  # Houses down by the river.
summary(chas)
houses_not_by_chas  = length(chas) - sum(chas)
houses_by_chas      = sum(chas)
print(paste("The number of houses sold by the Charles river is", houses_by_chas,
            "while the number of houses sold not by the Charles river is", houses_not_by_chas))
ans_10_e = "based on the number of distinct pupil-teacher ratios, tax rates, and crime rates 
        within chas==1, it looks like there are 9 suburbs in this dataset that border the
        Charles river."
print(ans_10_e)

# f)  What is the median pupil-teacher ratio in towns in this dataset.
unique_ptratio_rows = Boston[!duplicated(Boston[ , c('ptratio')]), ]
nrow(unique_ptratio_rows)

sorted_unique_ptratio_rows = unique_ptratio_rows[order(unique_ptratio_rows$ptratio), ]

    # Using median function
median(sorted_unique_ptratio_rows$ptratio)
    # Checking median function by hand.
(sorted_unique_ptratio_rows$ptratio[23] + sorted_unique_ptratio_rows$ptratio[24])/2

# g) Which suburb of Boston has lowest median value of owner-occupied homes? 
#   What are the values of the other predictors
#   for that suburb, and how do those values compare to the overall
#   ranges for those predictors? Comment on your findings.
Boston[ which(Boston$medv==min(Boston$medv)), ]  #  There is a tie.
#  Both have bad ptratio (20.2; max is 22)
#  Both have tax rates that are high (666)
#  Both have high crime rates (38.35 and 67.92, overall high is 88.)
#  To have cheap housing, you have to overpay in taxes, and you have bad schools, and high crime.

# h) In this data set, how many suburbs average more than 7 rooms per dwelling?  More than eight?  Comment on 8+
Boston_7plus = Boston[ which(Boston$rm > 7), ]
nrow(Boston_7plus)  # There are 64 observations with more than 7 rooms per dwelling.

Boston_8plus = Boston[ which(Boston$rm > 8), ]
nrow(Boston_8plus)  # There are 13 observations with more than 8 rooms per dwelling.
Boston_8plus
comments_on_8plus = "They all have low crime, low tax rates, few black people, and higher median values"

print(comments_on_8plus)