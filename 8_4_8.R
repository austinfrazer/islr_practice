# Chapter 8, Section 4, Number 8
# 
# In the lab, a classification tree was applied to the Carseats data set 
# after converting Sales into a qualitative response variable.  Now we
# will seek to predict Sales using regression trees and related approaches,
# treating the response as a quantitative variable.


library(tree)
library(ISLR)
library(randomForest)

# (a) Split the data set into a training set and a test set.

# For other computers, uncomment these lines to get it to work
#path = choose.dir()
#source(paste0(path, "\\my_sample.R"))

# On work computer, 
# path <- 'P:\\AccountFinance\\Planning\\Private\\Strategy&Loyalty\\Users\\Austin Frazer\\Data_Science_Practice\\ISLR_Labs\\islr_practice'
# source(paste0(path, "\\my_sample.R"))

# On black personal laptop
path <- 'C:\\Users\\Team Crazer\\Documents\\data_science_practice\\ISLR\\islr_practice'
source(paste0(path, "\\my_sample.R"))

set.seed(1)
train = my_sample(Carseats)
carseats_test = Carseats[-train, ]
carseats_train = Carseats[train, ]

head(Carseats)

# (b) Fit a regression tree to the training set.  Plot the tree, and
#     interpret the results.  What test MSE do you obtain?
tree_carseats = tree(Sales ~ ., data = Carseats, subset = train)
# tree_carseats   # Uncomment to see what tree_carseats raw output is.
summary(tree_carseats)

test_output = summary(tree_carseats)

plot(tree_carseats)
text(tree_carseats, pretty = 0)
print(paste0('This variable is actually used:  ', test_output$used, "."))

yhat_tree = predict(tree_carseats, newdata = Carseats[-train, ])
mse_tree = mean((yhat_tree - Carseats[-train, 'Sales']) ^ 2)

print(paste0("The MSE that I obtain is:  ", round(mse_tree, 3), "."))


# (c) Use cross-validation in order to determine the optimal level of
#     tree complexity.  Does pruning the tree improve the test MSE?


set.seed(3)
cv.carseats = cv.tree(tree_carseats, FUN = prune.tree)
names(cv.carseats)
#cv.carseats


par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")
print(paste0("The ideal number of terminal nodes ('size') is ", 
             cv.carseats$size[which.min(cv.carseats$dev)], "."))
print(paste0("The ideal value of the cost-complexity parameter, ",
             "k, is ", cv.carseats$k[which.min(cv.carseats$dev)], "."))

print(paste0("Pruning the tree does seem to give some slight improvements at first.  ",
      "After the first six cuts, there is a period of six or so more cuts that form a basin.",
      "  Pruning beyond that point leads to rapidly increasing error."))

# (d) Use the bagging approach in order to analyze this data.  What
#     test MSE do you obtain?  Use the importance() function to
#     determine which variables are most important.

bag_carseats = randomForest(Sales ~ .,
                            data = Carseats,
                            subset = train,
                            mtry = 10,
                            importance = TRUE)

bag_carseats

yhat_bag = predict(bag_carseats, newdata = carseats_test)

# Just added these two steps to make better axis-titles.
predicted_sales = yhat_bag
actual_sales = t(carseats_test['Sales'])

plot(predicted_sales, actual_sales)
abline(0, 1)
mean((yhat_bag - t(carseats_test['Sales']))^2)

# Just verifying that the aliases were done correctly.  Number should be the same as the above.
mean((predicted_sales - actual_sales)^2)

print("Here is the importance readout.")
importance(bag_carseats)

# (e) Use random forests to analyze this data.  What test MSE do you
#     obtain?  Use the importance() function to determine which variables
#     are most important.  Describe the effect of m, the number of
#     variables considered at each split, on the error rate obtained.


# First pass, just repeating what I did for the bagged model, but with 3 parameters
#  instead of all 10.
rf_carseats = randomForest(Sales ~ .,
                            data = Carseats,
                            subset = train,
                            mtry = 3,
                            importance = TRUE)

rf_carseats

yhat_rf = predict(rf_carseats, newdata = carseats_test)

# Just added these two steps to make better axis-titles.
predicted_sales = yhat_rf
actual_sales = t(carseats_test['Sales'])

plot(predicted_sales, actual_sales)
abline(0, 1)
mean((yhat_rf - t(carseats_test['Sales']))^2)

# Just verifying that the aliases were done correctly.
#  Number should be the same as the above.
mean((predicted_sales - actual_sales)^2)


# Repeating the same exercise, but with all possible number of variable choices (1, ..., p)
#  Also choosing 10 test - train splits of the data to make results more generalizable.
report_matrix = matrix(0, 10, 10)

for (i in 1:10){
  set.seed(i)
  train = my_sample(Carseats)
  carseats_test = Carseats[-train, ]
  carseats_train = Carseats[train, ]
for (j in 1:10){
    rf_carseats = randomForest(Sales ~ .,
                               data = Carseats,
                               subset = train,
                               mtry = i,
                               importance = TRUE)
    yhat_rf = predict(rf_carseats, newdata = carseats_test)    
    report_matrix[i, j] = mean((yhat_rf - t(carseats_test['Sales']))^2)
    print(paste0("I am on row ", i, " of 10 and column ", j, " of 10."))
}}


report_matrix
min(report_matrix)
which(report_matrix == min(report_matrix), arr.ind = TRUE)
report_matrix[6, 7]

# The above part may not be very useful though.  Each row represents a random split,
#  so we would not want to select that except maybe in very narrow teaching examples
#  or if we were trying to cheat at Kaggle or something.

# Instead, it may be more useful to look at the column means.  Each column mean is 
#  the average performance across each value of mtry which is the number of variables
#  in each random forest.  Averaging the performance of this across 10 different
#  random splits should improve the applicability of what we are finding.
mean_for_mtry = apply(report_matrix, MARGIN = 2, FUN = mean)
which.min(mean_for_mtry)
mean_for_mtry

# With this, we get that the lowest is the version with 8 parameters tried instead of 10
# as we did with just one sample of the data.