# 9. This problem involves the OJ data set which is part of the ISLR
# package.

library(ISLR)
library(tree)

# dim(OJ)
# head(OJ)

# (a) Create a training set containing a random sample of 800 observations,
# and a test set containing the remaining observations.

# On silver personal laptop
path <- "C:\\Users\\Austin\\Documents\\ISLR_Labs\\MyWork"
# On work computer, 
#   path <- 'P:\\AccountFinance\\Planning\\Private\\Strategy&Loyalty\\Users\\Austin Frazer\\Data_Science_Practice\\ISLR_Labs\\islr_practice'
# On black personal laptop
#   path <- 'C:\\Users\\Team Crazer\\Documents\\data_science_practice\\ISLR\\islr_practice'
# For other computers, uncomment these lines to get it to work
#   path = choose.dir()

source(paste0(path, "\\my_sample.R"))
set.seed(1)

train = my_sample(OJ, 800/nrow(OJ))
length(train)

OJ_test = OJ[-train, ]
OJ_train = OJ[train, ]


# (b) Fit a tree to the training data, with Purchase as the response
# and the other variables as predictors. Use the summary() function
# to produce summary statistics about the tree, and describe the
# results obtained. What is the training error rate? How many
# terminal nodes does the tree have?

tree_OJ = tree(Purchase ~ ., data = OJ, subset = train)
summary_tree_OJ = summary(tree_OJ)
#  print(summary_tree_OJ)  # Gives the raw output if more details are wanted. 

print(paste0('The training error rate is ', 
             summary_tree_OJ$misclass[1]/summary_tree_OJ$misclass[2],
             ', and the number of terminal nodes is ', summary_tree_OJ$size))

# (c) Type in the name of the tree object in order to get a detailed
# text output. Pick one of the terminal nodes, and interpret the
# information displayed.

tree_OJ
head(OJ)
??OJ
print(paste0("I will analyze the difference between node 24 and 25.  These split off from ",
              "node 12.  When the Discount Percentage of Minute Maid is greater than 19.6%",
             ", then Minute Maid is more likely to be purchased."))

# (d) Create a plot of the tree, and interpret the results.

plot(tree_OJ)
text(tree_OJ, pretty = 0)
print(paste0('This variable is actually used:  ', summary_tree_OJ$used, "."))

# (e) Predict the response on the test data, and produce a confusion
# matrix comparing the test labels to the predicted test labels.
# What is the test error rate?


pred_tree_OJ = predict(tree_OJ, OJ_test, type = "class")
Purchase_test = OJ_test$Purchase
my_tree  =  table(pred_tree_OJ, Purchase_test)
print("below is my confusion matrix.")
my_tree
misclassification_rate = round((my_tree[1, 2] + my_tree[2, 1]) / sum(my_tree),3)
print(paste0("My misclassification rate is:  ", misclassification_rate, "."))


# (f) Apply the cv.tree() function to the training set in order to
# determine the optimal tree size.


set.seed(3)
cv_OJ = cv.tree(tree_OJ, FUN = prune.tree)

# (g) Produce a plot with tree size on the x-axis and cross-validated
# classification error rate on the y-axis.

plot(cv_OJ$size, cv_OJ$dev, type = "b", xlab = "Tree Size", ylab = "Deviance")

# (h) Which tree size corresponds to the lowest cross-validated classification
# error rate?


print(paste0("The ideal number of terminal nodes ('size') is ", 
             cv_OJ$size[which.min(cv_OJ$dev)], "."))



# (i) Produce a pruned tree corresponding to the optimal tree size
# obtained using cross-validation. If cross-validation does not lead
# to selection of a pruned tree, then create a pruned tree with five
# terminal nodes.

# Did not show any improvement by tuning, so going with 5.
pruned_OJ = prune.tree(tree_OJ, best = 5)

# (j) Compare the training error rates between the pruned and unpruned
# trees. Which is higher?
pruned_error_rate = summary(pruned_OJ)$misclass[1]/summary(pruned_OJ)$misclass[2]
print(paste0("Pruned error rate is:  ", pruned_error_rate))

unpruned_error_rate = summary(tree_OJ)$misclass[1]/summary(tree_OJ)$misclass[2]
print(paste0("Unpruned error rate is:  ", unpruned_error_rate))

if(pruned_error_rate < unpruned_error_rate){
  print("Pruning offered an improvement in training error rates.")
  }else{print("Pruning failed to offer any improvement in training error rates.")}


# (k) Compare the test error rates between the pruned and unpruned
# trees. Which is higher?

pred_pruned_OJ = predict(pruned_OJ, OJ_test, type = "class")

my_tree  =  table(pred_pruned_OJ, Purchase_test)
print("below is my confusion matrix.")
my_tree
unpruned_misclassification_rate = misclassification_rate
pruned_misclassification_rate = round((my_tree[1, 2] + my_tree[2, 1]) / sum(my_tree),3)

if(pruned_misclassification_rate < unpruned_misclassification_rate){
  print("Pruning offered an improvement in test error rates.")
}else{print("Pruning failed to offer any improvement in test error rates.")}

pruned_misclassification_rate
unpruned_misclassification_rate