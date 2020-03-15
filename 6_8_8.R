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

Y = B0 + (B1 * X) + (B2 * (X^2)) + (B3 * (X^3)) + epsilon


# c) Use the regsubsets() function to perform best subset selection in order to choose the best model
#     containing the predictors X, X^2, ..., X^10.  What is the best model obtained according to
#     Cp, BIC, and adjusted R^2?  Show some plots to provide devidence for your answer,
#     and report the cefficients of the best model obtined.  Note that you will need to use the 
#     data.frame() function to create a single data set containing both X and Y.

mydata = data.frame(Y, X)


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

# The problem says to report the coefficients for the best model obtained.  I'm going to go with the 3-variable model. 
#  This is the one reported by two of the three tests, and for the third test (adj r2), there isn't much difference between 
#   the "best" (the one with 7 variables), and the 3-variable solution.

my_coef = coefficients(regfit, id = 3)
my_coef

print(paste0("recall that the original had B0 = ", B0, ", B1 = ", B1, ", B2 = ", B2, ", and B3 = ", B3))
print(paste0("The prediction has B0 = ", round(my_coef[1], 2),
             ", B1 = ", round(my_coef[2], 2), 
             ", B2 = ", round(my_coef[3], 2),
             ", and B3 = ", round(my_coef[4], 2)))