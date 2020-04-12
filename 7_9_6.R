# 6.  In this exercise, you will further analyze the Wage
# data set considered throughout this chapter.

# a) Perform polynomal regression to predict wage using age.
#   Use cross-validation to select the optimal degree d for
#   the polynomial.  What degree was chosen, and how does this
#   compare to the results of hypothesis testing using ANOVA?
#   Make a plot of the resulting polynomial fit to the data.

library(ISLR)



fit.20 = lm(wage ~ poly(age, 20), data = Wage)
round(coef(summary(fit.20)), 3)


### using the p-values, I would choose degree four for this data
# lm(wage ~ poly(age, 4), data = Wage)
### But, the question was about using cross-validation.


path = choose.dir()
source(paste0(path, "\\my_sample.R"))

report_matrix = matrix(0, 10, 10)
for (i in 1:10){
  train_rows = my_sample(Wage)
for (j in 1:10){
  assign(paste0("fit_", i, "_", j), lm(wage ~ poly(age, j), data = Wage, subset = train_rows))
  assign(paste0("mse_", i, "_", j), mean((Wage$wage - predict(get(paste0("fit_", i, "_", j)), Wage))[-train_rows] ^ 2))
  report_matrix[i, j] = get(paste0("mse_", i, "_", j))
}}


rownames(report_matrix) = c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5",
                               "sample_6", "sample_7", "sample_8", "sample_9", "sample_10")
colnames(report_matrix) = c("poly_1", "poly_2", "poly_3", "poly_4", "poly_5",
                            "poly_6", "poly_7", "poly_8", "poly_9", "poly_10")
report_matrix
average_mse = apply(report_matrix, MARGIN = 2, FUN = mean)
average_mse
which.min(average_mse)

# This gave me the same answer that the visual inspection of the p-values did; that is, a quartic model is the best here.


### Below, I try performing the same analyis but with 1000 samples instead of 10.
report_matrix_big = matrix(0, 1000, 10)
for (i in 1:1000){
  train_rows = my_sample(Wage)
  for (j in 1:10){
    assign(paste0("fit_", i, "_", j), lm(wage ~ poly(age, j), data = Wage, subset = train_rows))
    assign(paste0("mse_", i, "_", j), mean((Wage$wage - predict(get(paste0("fit_", i, "_", j)), Wage))[-train_rows] ^ 2))
    report_matrix_big[i, j] = get(paste0("mse_", i, "_", j))
  }}


dim(report_matrix_big)
colnames(report_matrix_big) = c("poly_1", "poly_2", "poly_3", "poly_4", "poly_5",
                            "poly_6", "poly_7", "poly_8", "poly_9", "poly_10")
report_matrix_big
average_mse_big = apply(report_matrix_big, MARGIN = 2, FUN = mean)
average_mse_big
which.min(average_mse_big)


# I could also try the k-fold cross validation technique given on page 193.
#  I went ahead and expanded it by repeating it 10 times with 10 different random seeds.

num_resamples = 10
num_folds     = 10
cv.error = matrix(0, num_resamples, num_folds)
for (j in 1:1000){
  seed_number = round(runif(1) * 2147483647, 0)     #Should be able to get all seeds except probably 0.  There are 2^32 seeds.
  set.seed(seed_number)
  print(paste0("Now using seed number, ", seed_number))
for (i in 1:10){
  glm.fit = glm(wage ~ poly(age, i), data = Wage)
  cv.error[j, i] = cv.glm(Wage, glm.fit, K = num_folds)$delta[1]
}}
cv.error
average_cv.error = apply(cv.error, MARGIN = 2, FUN = mean)
average_cv.error
which.min(average_cv.error)


# b) Fit a step function to predict wage using age, and perform
#   cross validation to choose the optimal number of cuts.  Make
#   a plot of the fit obtained.

#  Took from https://stackoverflow.com/questions/42190337/cross-validating-step-functions-in-r.
#   Found it when just looking for how to do cross-validation with a step function.
library(boot)
library(ISLR)
data(Wage) # using attach is a bad practice
set.seed(5082)
num_cuts = 12
cv.error <- rep (0, num_cuts)
for (i in 3:num_cuts){
  Wage$tmp <- cut(Wage$age, i)
  step.fit = glm(wage ~ tmp, data = Wage)
  cv.error[i] <- cv.glm(Wage ,step.fit, K= 10)$delta[1]
}
cv.error
cv.error[3:length(cv.error)]
print(paste0("The optimal number of cuts is:  ", which.min(cv.error[3:length(cv.error)]) + 2))


# Updating method from stackoverflow with something one of my techniques in part a.

num_resamples = 100
num_cuts = 12
num_folds = 10

cv.error = matrix(0, num_resamples, num_cuts)
for (j in 1:num_resamples){
  seed_number = round(runif(1) * 2147483647, 0)     #Should be able to get all seeds except probably 0.  There are 2^32 seeds.
  set.seed(seed_number)
  print(paste0("Now using seed number, ", seed_number))
  for (i in 3:num_cuts){
    Wage$tmp <- cut(Wage$age, i)
    step.fit = glm(wage ~ tmp, data = Wage)
    cv.error[j, i] <- cv.glm(Wage ,step.fit, K = num_folds)$delta[1]
  }}

useful_cv.error = cv.error[ , 3:ncol(cv.error)]
average_cv.error = apply(useful_cv.error, MARGIN = 2, FUN = mean)
average_cv.error
print(paste0("The optimal number of cuts is:  ", (which.min(average_cv.error) + 2)))
