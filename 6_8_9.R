# 9.  In this exercise, we will predict the number of applications received
# using the other variables in the College data set


library(ISLR)
fix(College)

# a)  Split the data into a test set and a training set.

path = choose.dir()
source(paste0(path, "\\my_sample.R"))

MY_DATA           = College
TRAIN_PROPORTION  = .5          # [0, 1)

train_obs = my_sample(MY_DATA, TRAIN_PROPORTION)


# b) Fit a linear model using least squares on the training set,
# and report the test error obtained.

college_lm = lm(Apps ~ ., data = MY_DATA, subset = train_obs)

attach(College)
MSE_college_lm = mean((Apps - predict(college_lm, College))[-train_obs]^2)
print(paste0("The mean squared error of the linear model is "
             , round(MSE_college_lm, 1)
             , "!"))

# (c)  Fit a ridge regression on the model on the training set, with lambda chosen
# by cross-validation. Report the test error obtained.
