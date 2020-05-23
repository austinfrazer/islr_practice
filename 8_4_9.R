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



# (f) Apply the cv.tree() function to the training set in order to
# determine the optimal tree size.



# (g) Produce a plot with tree size on the x-axis and cross-validated
# classification error rate on the y-axis.



# (h) Which tree size corresponds to the lowest cross-validated classification
# error rate?



# (i) Produce a pruned tree corresponding to the optimal tree size
# obtained using cross-validation. If cross-validation does not lead
# to selection of a pruned tree, then create a pruned tree with five
# terminal nodes.



# (j) Compare the training error rates between the pruned and unpruned
# trees. Which is higher?



# (k) Compare the test error rates between the pruned and unpruned
# trees. Which is higher?

