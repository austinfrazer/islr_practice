# 6.  In this exercise, you will further analyze the Wage
# data set considered throughout this chapter.

# a) Perform polynomal regression to predict wage using age.
#   Use cross-validation to select the optimal degree d for
#   the polynomial.  What degree was chosen, and how does this
#   compare to the results of hypothesis testing using ANOVA?
#   Make a plot of the resulting polynomial fit to the data.

library(ISLR)

lm(wage ~ poly(age, 4), data = Wage)

fit.20 = lm(wage ~ poly(age, 20), data = Wage)
coef(summary(fit.20))


# b) Fit a step function to predict wage using age, and perform
#   cross validation to choose the optimal number of cuts.  Make
#   a plot of the fit obtained.

