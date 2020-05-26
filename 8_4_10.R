# 10. We now use boosting to predict Salary in the Hitters data set.

library(ISLR) # has Hitters data set.

# (a) Remove the observations for whom the salary information is
# unknown, and then log-transform the salaries.

#str(Hitters)
#str(Hitters$Salary)
#dim(Hitters)
#sum(is.na(Hitters$Salary))
Hitters = Hitters[which(Hitters$Salary != 'NA'), ]
#dim(Hitters)

# (b) Create a training set consisting of the first 200 observations, and
# a test set consisting of the remaining observations.

training_hitters = Hitters[1:200, ]
test_hitters = Hitters[201:nrow(Hitters), ]
dim(training_hitters)
dim(test_hitters)

# (c) Perform boosting on the training set with 1,000 trees for a range
# of values of the shrinkage parameter λ. Produce a plot with
# different shrinkage values on the x-axis and the corresponding
# training set MSE on the y-axis.



# (d) Produce a plot with different shrinkage values on the x-axis and
# the corresponding test set MSE on the y-axis.



# (e) Compare the test MSE of boosting to the test MSE that results
# from applying two of the regression approaches seen in
# Chapters 3 and 6.



# (f) Which variables appear to be the most important predictors in
# the boosted model?



# (g) Now apply bagging to the training set. What is the test set MSE
# for this approach? 


