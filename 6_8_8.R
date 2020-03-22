#8.  In this exercise, we will generate simulated data, and will then use this data to perform best subset selection.

# a)  Use the rnorm() function to generate a predictor X of length n = 100, as well as a noise vector epsilon of length n = 100.
n = 100
X = rnorm(n)
x <- X             # duplicating "X" as "x" to avoid typing confusion later.
epsilon = rnorm(n)

# b)  Generate a response vector Y of length n = 100 according to the model
    #     Y = B0 + B1X + B2X^2 + B3X^3 + epsilon
    #             where B0, B1, B2, and B3 are constants of your choice.

B0 = 25
B1 = 10
B2 = 50
B3 = 100

# # Generated these to generate bigger differences in the predictions of the different methods.  It worked.
# B0 = rnorm(1)
# B1 = rnorm(1)
# B2 = rnorm(1)
# B3 = rnorm(1)

Y = B0 + (B1 * X) + (B2 * (X^2)) + (B3 * (X^3)) + epsilon


# c) Use the regsubsets() function to perform best subset selection in order to choose the best model
#     containing the predictors X, X^2, ..., X^10.  What is the best model obtained according to
#     Cp, BIC, and adjusted R^2?  Show some plots to provide devidence for your answer,
#     and report the cefficients of the best model obtined.  Note that you will need to use the 
#     data.frame() function to create a single data set containing both X and Y.

mydata = data.frame(Y, X)

# This package contains regsubsets
library(leaps)

regfit = regsubsets(Y ~ poly(X, 10, raw = T), data = mydata, nvmax = 10)
summary(regfit)

reg.summary = summary(regfit)
print(reg.summary)
names(reg.summary)


# Per Cp?
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Mallow's Cp", type = "l")

# per BIC?
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "Bayesian Information Criterion", type = "l")

# per adj-rsq?
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R-squared", type = "l")

# compare the three
par(mfrow=c(2,2))
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Mallow's Cp", type = "l")
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "Bayesian Information Criterion", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R-squared", type = "l")


cp_answer = which.min(reg.summary$cp)
print(paste0("for Mallow's Cp, the best model is the one with ", cp_answer, " variables."))

bic_answer = which.min(reg.summary$bic)
print(paste0("for the Bayesian Information Criterion, the best model is the one with ", bic_answer, " variables."))

adjrsq_answer = which.max(reg.summary$adjr2)
print(paste0("for Adjusted R-squared, the best model is the one with ", adjrsq_answer, " variables."))

# Using function from  (https://www.tutorialspoint.com/r/r_mean_median_mode.htm) to get mode.
#   Will use this to choose mode from three tests and use it to determine "best"
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

possible_answers = c(cp_answer, bic_answer, adjrsq_answer)

best_number_of_variables = getmode(possible_answers)
print(paste0("the best number of variables is ", best_number_of_variables))


######## The result narrated below was the first result I obtained and the most common result I obtained
########  after 10 or so attempts.
# The problem says to report the coefficients for the best model obtained.  I'm going to go with the 3-variable model. 
#  This is the one reported by two of the three tests, and for the third test (adj r2), there isn't much difference between 
#   the "best" (the one with 7 variables), and the 3-variable solution.


my_coef = coefficients(regfit, id = best_number_of_variables)
my_coef

print(paste0("recall that the original had B0 = ", B0, ", B1 = ", B1, ", B2 = ", B2, ", and B3 = ", B3))
print(paste0("The prediction has B0 = ", round(my_coef[1], 2),
             ", B1 = ", round(my_coef[2], 2), 
             ", B2 = ", round(my_coef[3], 2),
             ", and B3 = ", round(my_coef[4], 2)))


# d) Repeat (c), using forward stepwise selection and also using backwards stepwise selection.
# How does your answer compare to the results in (c)

# Using forward stepwise selection.
regfit.fwd = regsubsets(Y ~ poly(X, 10, raw = T), data = mydata, nvmax = 10, method = "forward")
regfit.fwd.summ = summary(regfit.fwd)

# Using backward stepwise selection.
regfit.bwd = regsubsets(Y ~ poly(X, 10, raw = T), data = mydata, nvmax = 10, method = "backward")
regfit.bwd.summ = summary(regfit.bwd)



# Creating a function from earlier one-off work.
getting_best_model = function(my_model_summary, my_model){

# Per Cp?
plot(my_model_summary$cp, xlab = "Number of Variables", ylab = "Mallow's Cp", type = "l")
# per BIC?
plot(my_model_summary$bic, xlab = "Number of Variables", ylab = "Bayesian Information Criterion", type = "l")
# per adj-rsq?
plot(my_model_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R-squared", type = "l")
# compare the three
par(mfrow=c(2,2))
plot(my_model_summary$cp, xlab = "Number of Variables", ylab = "Mallow's Cp", type = "l")
plot(my_model_summary$bic, xlab = "Number of Variables", ylab = "Bayesian Information Criterion", type = "l")
plot(my_model_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R-squared", type = "l")


cp_answer = which.min(my_model_summary$cp)
print(paste0("for Mallow's Cp, the best model is the one with ", cp_answer, " variables."))

bic_answer = which.min(my_model_summary$bic)
print(paste0("for the Bayesian Information Criterion, the best model is the one with ", bic_answer, " variables."))

adjrsq_answer = which.max(my_model_summary$adjr2)
print(paste0("for Adjusted R-squared, the best model is the one with ", adjrsq_answer, " variables."))

possible_answers = c(cp_answer, bic_answer, adjrsq_answer)

best_number_of_variables = getmode(possible_answers)
print(paste0("the best number of variables is ", best_number_of_variables))
my_coef = coefficients(my_model, id = best_number_of_variables)
my_coef

print(paste0("recall that the original had B0 = ", B0, ", B1 = ", B1, ", B2 = ", B2, ", and B3 = ", B3))
print(paste0("The prediction has B0 = ", round(my_coef[1], 2),
             ", B1 = ", round(my_coef[2], 2), 
             ", B2 = ", round(my_coef[3], 2),
             ", and B3 = ", round(my_coef[4], 2)))
return(my_coef)
}

print("FOR FORWARD STEPWISE SELECTION:")
forward_answer = getting_best_model(regfit.fwd.summ, regfit.fwd)
print("FOR BACKWARD STEPWISE SELECTION:")
backward_answer = getting_best_model(regfit.bwd.summ, regfit.bwd)
print("FOR BEST SUBSET SELECTION:")
best_answer = getting_best_model(reg.summary, regfit)

forward_answer
backward_answer
best_answer

# The answers appear to be the same across all three methods.


# e) Now fit a lasso model to the simulated data, again using X, X^2, ..., X^10 as predictors.
# Use cross-validation to select the optimal value of lambda.  Create plots of the cross-validation
# error as a function of lambda.  Report the resulting coefficient estimates, and discuss the results
# obtained.


# I took a lot from the solutions manual link here as I got stuck for awhile.

# Use cross-validation to select the optimal value of lambda.
library(glmnet)

xmat = model.matrix(Y ~ poly(x, 10, raw = T), data = mydata)[, -1]
lasso.mod = cv.glmnet(xmat, Y, alpha = 1)

lasso.mod
min(lasso.mod$lambda)
best.lambda = lasso.mod$lambda.min
best.lambda


# Create plots of the cross-validation error as a function of lambda.
dev.off()
plot(lasso.mod)
log(best.lambda)  # just to confirm plot is picking up the right answer.


# Report the resulting coefficient estimates, and discuss the results obtained.
best.model = glmnet(xmat, Y, alpha = 1)
predict(best.model, s = best.lambda, type = "coefficients")

my_output = predict(best.model, s = best.lambda, type = "coefficients")

   #  The first run had weird results, so I regenerated the data a few times.
   #    The results aren't too far off.  (32.8, 11.6, 41.4, 96.9) vs (25, 10, 50, 100)



# (f) Now generate a response vector Y according to the model
#   Y = B0 + B7X^7 + epsilon,
# and perform best subset selection and the lasso.  Discuss the results obtained.

B7 = 7000
Y2 = B0 + B7*(X^7) + epsilon

my_data2 = data.frame(y = Y2, x = X)
mod.full2 = regsubsets(y ~ poly(x, 10, raw = T), data = my_data2, nvmax = 10)
mod.summary2 = summary(mod.full2)


print("FOR BEST SUBSET SELECTION on new data:")
best_answer2 = getting_best_model(mod.summary2, mod.full2)

best_answer2

#### Taking the method from c outside the function I built in d just to see it line-by-line
# # Per Cp?
# plot(mod.summary2$cp, xlab = "Number of Variables", ylab = "Mallow's Cp", type = "l")
# 
# # per BIC?
# plot(mod.summary2$bic, xlab = "Number of Variables", ylab = "Bayesian Information Criterion", type = "l")
# 
# # per adj-rsq?
# plot(mod.summary2$adjr2, xlab = "Number of Variables", ylab = "Adjusted R-squared", type = "l")
# 
# # compare the three
# par(mfrow=c(2,2))
# plot(mod.summary2$cp, xlab = "Number of Variables", ylab = "Mallow's Cp", type = "l")
# plot(mod.summary2$bic, xlab = "Number of Variables", ylab = "Bayesian Information Criterion", type = "l")
# plot(mod.summary2$adjr2, xlab = "Number of Variables", ylab = "Adjusted R-squared", type = "l")
# 
# 
# cp_answer = which.min(mod.summary2$cp)
# print(paste0("for Mallow's Cp, the best model is the one with ", cp_answer, " variables."))
# 
# bic_answer = which.min(mod.summary2$bic)
# print(paste0("for the Bayesian Information Criterion, the best model is the one with ", bic_answer, " variables."))
# 
# adjrsq_answer = which.max(mod.summary2$adjr2)
# print(paste0("for Adjusted R-squared, the best model is the one with ", adjrsq_answer, " variables."))

####  Now, lasso.

xmat2 = model.matrix(Y2 ~ poly(x, 10, raw = T), data = my_data2)[, -1]
lasso.mod2 = cv.glmnet(xmat2, Y2, alpha = 1)



lasso.mod2
min(lasso.mod2$lambda)
best.lambda2 = lasso.mod2$lambda.min
best.lambda2


# Create plots of the cross-validation error as a function of lambda.
dev.off()
plot(lasso.mod)
log(best.lambda2)  # just to confirm plot is picking up the right answer.


# Report the resulting coefficient estimates, and discuss the results obtained.
best.model2 = glmnet(xmat2, Y2, alpha = 1)
predict(best.model2, s = best.lambda2, type = "coefficients")

my_output2 = predict(best.model2, s = best.lambda2, type = "coefficients")

#  The results from the lasso look great.  The intercept is far off, but the 
#  B7 coefficient is really close.

#  The best subset selection results look like they are better.  That isn't too surprising, 
#  since 7000 is an extreme value relative to the small intercept.  Lasso performs better
#  when the variables have been normalized.