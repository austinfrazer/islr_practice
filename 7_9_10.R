# 10. This question relates to the College data set.

# (a) Split the data into a training set and a test set. Using out-of-state
# tuition as the response and the other variables as the predictors,
# perform forward stepwise selection on the training set in order
# to identify a satisfactory model that uses just a subset of the
# predictors.

library(ISLR)
library(leaps)

path = choose.dir()
source(paste0(path, "\\my_sample.R"))

dim(College)
head(College)

train_rows = my_sample(College)

# I am sure there is a more straightforward way to get to the test_rows somewhere.
test_rows = 1:nrow(College)
for (i in 1:length(test_rows)){
  if (i %in% train_rows){
    test_rows[i] = NA
  }
}
test_rows = test_rows[!is.na(test_rows)]

numvar = ncol(College) - 1

regfit_fwd = regsubsets(Outstate ~ ., data = College[train_rows, ], nvmax = numvar, method = "forward")
test_mat = model.matrix(Outstate ~ ., data = College[test_rows, ])

regfit_fwd$np
val_errors = rep(NA, regfit_fwd$np - 1)
for (i in 1:(regfit_fwd$np - 1)){
  coefi = coef(regfit_fwd, id = i)
  pred  = test_mat[, names(coefi)] %*% coefi
  val_errors[i] = mean((College$Outstate[test_rows] - pred)^2)
}

val_errors
plot(val_errors)
chosen_model_size = which.min(val_errors)
coef(regfit_fwd, chosen_model_size)


# My method, cribbed from the chapter 6 lab on forward stepwise selection, ended up
#  choosing all possible variables.  Though this is a possible solution, it makes 
#  non-linear experimentation too difficult.  I instead will choose the six
#  parameter model like was found in 
#  (https://blog.princehonest.com/stat-learning/ch7/10.html)

output_a = coef(regfit_fwd, 6)

# (b) Fit a GAM on the training data, using out-of-state tuition as
# the response and the features selected in the previous step as
# the predictors. Plot the results, and explain your findings.

names(output_a)

library(gam)

# Practicing from lab on pages 294- 295
gam_m3 = gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
par(mfrow = c(1, 3))
plot(gam_m3, se = TRUE, col = "blue")

# attempt to get variables to automatically generate from output of forward
#  stepwise selection, unfortunately, failed.
# output_a[-1]
# r22 = paste0(regressors, collapse=" + ")
# r22
# regressors = names(output_a[-1])
# regressors
# regressors
# noquote(r22)
# r22
# r22
# head(College, 1)
# lm(Outstate ~ Private + Apps + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate, data = College)
# gam(Outstate ~ names(output_a[-1]), data = College, subset = train_rows)

# With time running out, just basically went straight to solutions manual.
gam_fit = gam(Outstate ~ Private + s(Room.Board, df = 2) + s(PhD, df = 2) +
                s(perc.alumni, df = 2) + s(Expend, df = 5) + s(Grad.Rate, df = 2),
              data = College[train_rows, ])
par(mfrow = c(2, 3))
plot(gam_fit, se = T, col = "blue")


# (c) Evaluate the model obtained on the test set, and explain the
# results obtained.

#  Low on time, just used solutions manual result.

gam_pred = predict(gam_fit, College[test_rows, ])
gam_err = mean((College$Outstate[test_rows] - gam_pred) ^ 2)
gam_err

gam_tss = mean((College$Outstate[test_rows] - mean(College$Outstate[test_rows]))^2)
test_rss = 1 - gam_err/gam_tss
test_rss





# (d) For which variables, if any, is there evidence of a non-linear
# relationship with the response?

summary(gam_fit)
print(paste0("Non-parametric Anova test shows a strong evidence",
             "of non-linear relationship between response and Expend,", 
             "and a moderately strong non-linear relationship (using p",
             "value of 0.05) between response and Grad.Rate or PhD."))