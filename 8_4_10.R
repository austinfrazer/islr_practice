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
Hitters$Salary = log(Hitters$Salary)

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

library(gbm)

set.seed(3)
boost_hitters = gbm(Salary ~ .,
                    data = training_hitters,
                    distribution = "gaussian",
                    n.trees = 1000,
                    shrinkage = .5
                    )

print(paste0("The training error for this gradient boosting machine is:  ",
             round(boost_hitters$train.error[boost_hitters$n.trees], 5)))
boost_hitters$train.error

### I'm not sure if this is a way to find the best number of trees or not.
# min(boost_hitters$train.error)
# which.min(boost_hitters$train.error)

i_max = 10
j_max = 100
report_matrix = matrix(0, i_max, j_max)

for (i in 1:i_max){
  set.seed(i)
  for (j in 1:j_max){
    boost_hitters = gbm(Salary ~ .,
                        data = training_hitters,
                        distribution = "gaussian",
                        n.trees = 1000,
                        shrinkage = (j * 0.001)
                        )    
    train_MSE = boost_hitters$train.error[boost_hitters$n.trees]
    report_matrix[i, j] = train_MSE
    print(paste0("I am on row ", i, " of ", i_max, " and column ", j, " of ", j_max, "."))
  }}

report_matrix
min(report_matrix)
which(report_matrix == min(report_matrix), arr.ind = TRUE)
report_matrix[which(report_matrix == min(report_matrix), arr.ind = TRUE)]

# The above part may not be very useful though.  Each row represents a random split,
#  so we would not want to select that except maybe in very narrow teaching examples
#  or if we were trying to cheat at Kaggle or something.

# Instead, it may be more useful to look at the column means.  Each column mean is 
#  the average performance across each value of mtry which is the number of variables
#  in each random forest.  Averaging the performance of this across 10 different
#  random splits should improve the applicability of what we are finding.
mean_for_shrinkage = apply(report_matrix, MARGIN = 2, FUN = mean)
which.min(mean_for_shrinkage)
mean_for_shrinkage
print(paste0("The best value for the shrinkage parameter is:  ",
             which.min(mean_for_shrinkage) * .001,
             "."))

train_MSE = mean_for_shrinkage
shrinkage_parameter = 1:length(mean_for_shrinkage) * .001
plot_data_frame = data.frame(shrinkage_parameter, train_MSE)

plot(plot_data_frame)

# (d) Produce a plot with different shrinkage values on the x-axis and
# the corresponding test set MSE on the y-axis.

i_max = 10
j_max = 100
report_matrix = matrix(0, i_max, j_max)

for (i in 1:i_max){
  set.seed(i)
  for (j in 1:j_max){
    boost_hitters = gbm(Salary ~ .,
                        data = training_hitters,
                        distribution = "gaussian",
                        n.trees = 1000,
                        shrinkage = (j * 0.001)
    )    
    yhat.boost = predict(boost_hitters,
                         newdata = test_hitters,
                         n.trees = 1000)
    test_MSE = mean((yhat.boost - test_hitters$Salary) ^ 2)
    report_matrix[i, j] = test_MSE
    print(paste0("I am on row ", i, " of ", i_max, " and column ", j, " of ", j_max, "."))
  }}

report_matrix
min(report_matrix)
which(report_matrix == min(report_matrix), arr.ind = TRUE)#10, 91
report_matrix[which(report_matrix == min(report_matrix), arr.ind = 
TRUE)]

mean_for_shrinkage = apply(report_matrix, MARGIN = 2, FUN = mean)
which.min(mean_for_shrinkage)
mean_for_shrinkage
print(paste0("The best value for the shrinkage parameter is:  ",
             which.min(mean_for_shrinkage) * .001,
             "."))

MSE = mean_for_shrinkage
shrinkage_parameter = 1:length(mean_for_shrinkage) * .001
plot_data_frame = data.frame(shrinkage_parameter, MSE)

plot(plot_data_frame)

# (e) Compare the test MSE of boosting to the test MSE that results
# from applying two of the regression approaches seen in
# Chapters 3 and 6.

MSE_gbm = min(mean_for_shrinkage)

   # From chapter 3
ols_hitters = lm(Salary ~ .,
                 data = training_hitters)
y_hat = predict(ols_hitters, newdata = test_hitters)
MSE_ols = mean((y_hat - test_hitters$Salary)^2)

   # From chapter 6
library(glmnet)

x_train = model.matrix(object = as.formula("Salary ~ ."),
                 data = training_hitters)[ , -1]
x_test = model.matrix(object = as.formula("Salary ~ ."),
                       data = test_hitters)[ , -1]
y_train = training_hitters$Salary
y_test  = test_hitters$Salary
grid = 10^seq(10,-2, length = 100)
ridge.mod = glmnet(x = x_train,
                   y = y_train,
                    alpha = 0,
                    lambda = grid ,
                    thresh = 1e-12)
ridge.pred = predict(ridge.mod, s = 4, newx = x_test)
MSE_ridge = mean((ridge.pred -y_test)^2)

cat(paste0("MSE_gbm = ", MSE_gbm,
                 "\nMSE_ols = ", MSE_ols,
                 "\nMSE_ridge = ", MSE_ridge))

# (f) Which variables appear to be the most important predictors in
# the boosted model?
print("Here are all of the variables in the model and their relative importances:  ")
summary(boost_hitters)

print("Here are the top-5 variables in the model and their relative importances:  ")
summary(boost_hitters)[1:5, ]

print(paste0("Career at-Bats is the highest predictor of salary.  This kind of proxies for how long someone has been",
      " in the league as well as if they are decent.  A long-time backup player wouldn't have a high value for this, ",
      "but neither would a rookie."))


# (g) Now apply bagging to the training set. What is the test set MSE
# for this approach?

library(randomForest)
bag_hitters = randomForest(Salary ~ .,
                           data = training_hitters,
                           mtry = ncol(training_hitters) - 1,
                           importance = TRUE)


y_hat_bag = predict(bag_hitters, newdata = test_hitters)
MSE_bag = mean((y_hat_bag - test_hitters$Salary)^2)


cat(paste0("MSE_gbm = ", MSE_gbm,
           "\nMSE_ols = ", MSE_ols,
           "\nMSE_ridge = ", MSE_ridge,
           "\nMSE_bag = ", MSE_bag))

print("I'm not sure why the bagged model is better than the gbm here....")

