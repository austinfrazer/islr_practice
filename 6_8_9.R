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

if(MSE_college_lm > MSE_college_ridge){
  msg = "Ridge regression performed better than OLS!"
} else if (MSE_college_lm == MSE_college_ridge){
  msg = "Ridge and OLS performed the same."
} else {
  msg = "OLS performed better than Ridge???"
}

print(paste0("The mean squared error of the ridge regression model is "
             , round(MSE_college_ridge, 1)
             , "!"))

print(msg)


# (d)  Fit a lasso model on the model on the training set, with lambda chosen
# by cross-validation. Report the test error obtained, along with the
# number of non-zero coefficient estimates.


college_lasso = cv.glmnet(train_matrix,
                          college_train[ , "Apps"],
                          alpha = 1,
                          lambda = grid,
                          thresh = 1e-12)
lambda_best_lasso   = college_lasso$lambda.min

lasso_pred    = predict(college_lasso, newx = test_matrix, s = lambda_best_lasso)
number_of_nonzero_coefficients = nrow(predict(  college_lasso,
                                                type = "nonzero",
                                                newx = test_matrix,
                                                s = lambda_best_lasso
                                              )
                                      )
# The answer can also be seen by just entering the name of the model and running it.
#  "college_lasso", for example.  But the result easily displayed there is not
#  callable or savable, unfortunately.

MSE_college_lasso = mean((college_test[ , "Apps"] - lasso_pred)^2)

if(MSE_college_lm > MSE_college_lasso){
  msg = "lasso regression performed better than OLS!"
} else if (MSE_college_lm == MSE_college_lasso){
  msg = "lasso and OLS performed the same."
} else {
  msg = "OLS performed better than lasso???"
}

print(paste0("The mean squared error of the lasso regression model is "
             , round(MSE_college_lasso, 1)
             , "!"))

print(msg)
print(paste0("The number of nonzero coefficients for this lasso model is ",
             number_of_nonzero_coefficients))

#  e) Fit a PCR model on the training set, with M chosen by cross-validation.
#   Report the test error obtained, along with the value of M selected by
#    cross-validation.

library(pls)
college_pcr = pcr(Apps ~ .,
              data = MY_DATA,
              subset = train_obs,
              scale = TRUE, 
              validation = "CV")
validationplot(college_pcr, val.type = "MSEP")
summary(college_pcr)

  #  Based on visual inspection, I'm going to choose 5.
  #   I can see from looking at the summary that the best is 17,
  #   but that is basically just replicating the regular linear model
  #  Instead, I am going to go with the second big kink in the graph.
chosen_M = 5

pcr_predictions = predict(college_pcr, College[-train_obs, ], ncomp = chosen_M)


MSE_college_pcr = mean((Apps[-train_obs] - pcr_predictions)^2)
print(paste0("The mean squared error of the pcr model with ",
             chosen_M, " components is " 
             , round(MSE_college_pcr, 1), "!"))


# The error on this one is substantially (+2x) bigger than the other models.
#  I did choose a suboptimal M value.

# When I run it again for the optimal value, 17, I get the linear model's results.
pcr_pred17 = predict(college_pcr, College[-train_obs, ], ncomp = 17)
MSE_college_pcr17 = mean((Apps[-train_obs] - pcr_pred17)^2)
MSE_college_pcr17


# f)  Fit a PLS model on the training set, with M chosen by cross-validation.
#       Report the test error obtained, along withthe value of M selected by
#       cross validation.

college_pls = plsr(Apps ~ .,
                  data = MY_DATA,
                  subset = train_obs,
                  scale = TRUE, 
                  validation = "CV")
validationplot(college_pls, val.type = "MSEP")
summary(college_pls)

# I'm choosing 8.  It is tied for 9 as the best option.
chosen_M = 8

pls_predictions = predict(college_pls, College[-train_obs, ], ncomp = chosen_M)

MSE_college_pls = mean((Apps[-train_obs] - pls_predictions)^2)
MSE_college_pls

# This one looks pretty good.

print(paste0("The mean squared error of the pls model with ",
             chosen_M, " components is "
             , round(MSE_college_pls, 1)
             , "!"))

# g)  Comment on the results obtained.  How accurately can we 
# predict the number of college applications received?  Is 
# there much difference among the test errors resulting from these
# five approaches?


MSE_mean_model = mean((Apps[-train_obs] - mean(Apps[train_obs]))^2)
MSE_mean_model



cat("The MSE for the models tested are as follows:\nLinear Model ", 
    MSE_college_lm, 
    "\nRidge Regression ",
    MSE_college_ridge,
    "\nLasso Regression ",
    MSE_college_lasso,
    "\nPCR model ",
    MSE_college_pcr,
    "\nPLS model ",
    MSE_college_pls)


print(paste0("There aren't any substantial differences between them ",
             "except for the PCR model which was done on a suboptimal M.",
             "  All of them are better than the mean model, with the ",
             "mean model being defined as the mean application number",
             " of the training group being subtracted from the Apps value ",
             "from the test group.  That value is ", 
             round(MSE_mean_model, 0),
             " which is substantially bigger than any of the others."))

cat("The MSE improvement for the models tested are as follows:\nLinear Model ", 
    MSE_mean_model/MSE_college_lm,
    "\nRidge Regression ",
    MSE_mean_model/MSE_college_ridge,
    "\nLasso Regression ",
    MSE_mean_model/MSE_college_lasso,
    "\nPCR model ",
    MSE_mean_model/MSE_college_pcr,
    "\nPLS model ",
    MSE_mean_model/MSE_college_pls)

print(my_print_statement)