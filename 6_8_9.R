# 9.  In this exercise, we will predict the number of applications received
# using the other variables in the College data set


library(ISLR)

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

library(glmnet)

college_train = College[train_obs, ]
college_test  = College[-train_obs, ]

train_matrix  = model.matrix(Apps ~ ., data = college_train)
test_matrix   = model.matrix(Apps ~ ., data = college_test)

grid = 10 ^ seq(10, -2, length = 100)
college_ridge = cv.glmnet(train_matrix,
                          college_train[ , "Apps"],
                          alpha = 0,
                          lambda = grid,
                          thresh = 1e-12)
lambda_best   = college_ridge$lambda.min

ridge_pred    = predict(college_ridge, newx = test_matrix, s = lambda_best)

MSE_college_ridge = mean((college_test[ , "Apps"] - ridge_pred)^2)

print(paste0("The mean squared error of the ridge regression model is "
             , round(MSE_college_ridge, 1)
             , "!"))