# Chapter 8, Section 4, Number 8
# 
# In the lab, a classification tree was applied to the Carseats data set 
# after converting Sales into a qualitative response variable.  Now we
# will seek to predict Sales using regression trees and related approaches,
# treating the response as a quantitative variable.


library(tree)
library(ISLR)

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






# Relevant section from Ch. 8 lab.



library(tree)
library(ISLR)
attach(Carseats)
High = ifelse(Sales <= 8, "No", "Yes")
Carseats = data.frame(Carseats, High)
tree.carseats = tree(High ~ .  -  Sales, Carseats)
summary(tree.carseats)



plot(tree.carseats)
text(tree.carseats, pretty = 0)



tree.carseats



set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[ - train, ]
High.test = High[ - train]
tree.carseats = tree(High ~ . - Sales, Carseats, subset = train)
tree.pred = predict(tree.carseats, Carseats.test, type = "class")
#These lines below redesigned by me because the hardcoded values, (86 + 57)/200, were no longer relevant.
my_tree  =  table(tree.pred, High.test)
my_tree
(my_tree[1, 1] + my_tree[2, 2]) / sum(my_tree)



set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats



par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")



prune.carseats = prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)



tree.pred = predict(prune.carseats, Carseats.test, type = "class")
# Edited code below to update.  Fixed values, (94 + 60)/200, were incorrect, 
my_tree  =  table(tree.pred, High.test)
my_tree
(my_tree[1, 1] + my_tree[2, 2]) / sum(my_tree)



prune.carseats = prune.misclass(tree.carseats, best = 15)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred = predict(prune.carseats, Carseats.test, type = "class")
# Edited code below to update.  Fixed values, (86 + 62)/200, were incorrect, 
table(tree.pred, High.test)
my_tree  =  table(tree.pred, High.test)
my_tree
(my_tree[1, 1] + my_tree[2, 2]) / sum(my_tree)






# For other computers, uncomment these lines to get it to work
#path = choose.dir()
#source(paste0(path, "\\my_sample.R"))

# On work computer, 
path <- 'P:\\AccountFinance\\Planning\\Private\\Strategy&Loyalty\\Users\\Austin Frazer\\Data_Science_Practice\\ISLR_Labs\\islr_practice'
source(paste0(path, "\\my_sample.R"))

library(MASS)
library(randomForest)


# I'll use a fixed seed so I have the same sample.  (I could loop through this later so I have 10 or 100 samples or so.)
set.seed(1)
train = my_sample(Boston)
boston.test = Boston[ - train, "medv"]

report_matrix = matrix(0, 13, 200)
for (i in 1:13){
  for (j in 1:200){
    bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=i,ntree=(j*25))
    yhat.bag = predict(bag.boston,newdata=Boston[-train,])    
    report_matrix[i, j] = mean((yhat.bag-boston.test)^2)
    print(paste0("I am on row ", i, " of 13 and column ", j, " of 200."))
  }}


min(report_matrix)

which(report_matrix == min(report_matrix), arr.ind = TRUE)

report_matrix[2, 3]




report_dataframe = data.frame(report_matrix)

rownames(report_dataframe)
orig_colnames = colnames(report_dataframe)
#orig_colnames

new_colnames = sub(".", "", colnames(report_dataframe))
#new_colnames
transformed_colnames = as.numeric(new_colnames) * 25
#transformed_colnames
colnames(report_dataframe) <- transformed_colnames

names(report_dataframe)

library(tidyr)


num_param <- rownames(report_dataframe)

report_dataframe$num_param = num_param


df <- pivot_longer(report_dataframe,
                   cols = 1:(ncol(report_dataframe) - 1),
                   names_to = "num_trees",
                   values_to = "mse")

df

library(rgl)


# Plot
par(mar=c(0,0,0,0))
plot3d( 
  x = df$num_param,
  y = df$num_trees,
  z = df$mse,
  type = 's',
  radius = 10,
  xlab="Number of Parameters", ylab="Number of Trees", zlab="Mean-Squared Error")
writeWebGL( filename="HtmlWidget/3dscatter.html" ,  width=600, height=600)


# Library
library(plotly)


q <- plot_ly(z = report_matrix, type = "surface")
q

result_location
result_location[1]
result_location[2]

which(report_matrix == min(report_matrix), arr.ind = TRUE)
result = round(min(report_matrix), 3)
result_location = which(report_matrix == min(report_matrix), arr.ind = TRUE)

print(paste0("The best result, ", result, " is found with num_param = ", result_location[1], " and num_trees = ",
            result_location[2] * 25, ". This is a little surprising because it is not using that many trees.  ",
            "This result may change if one took multiple samples on different ",
             "seeds instead of just using one sample from one random seed."))