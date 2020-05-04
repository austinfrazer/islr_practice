# Chapter 8, Section 4, Number 7
# 
# In the lab, we applied random forests to the Boston data using mtry=6
# and using ntree=25 and ntree=500.  Create a plot displaying the test
# error resulting from random forests on this data set for a more
# comprehensive range of values for mtry and ntree.  You can model your
# plot after Figure 8.10.  Describe the results obtained.

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