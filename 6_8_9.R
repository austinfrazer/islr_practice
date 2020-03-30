# 9.  In this exercise, we will predict the number of applications received
# using the other variables in the College data set


library(ISLR)
fix(College)

# a)  Split the data into a test set and a training set.

path = choose.dir()
source(paste0(path, "\\my_sample.R"))

MY_DATA         = College
TRAIN_PROPORTION = .5          # [0, 1)

test_obs = my_sample(MY_DATA, TEST_PROPORTION)
train_obs = nrow(MY_DATA)

# b) Fit a linear model using least squares on the training set,
# and report the test error obtained.

lm(Apps ~ ., data = MY_DATA, subset = train)
names(MY_DATA)
MY_DATA[-test_obs, ]
MY_DATA[test_obs, ]

train=sample(392,196)
train

lm(mpg~horsepower, data=Auto, subset = train)
lm(mpg ~ ., data=Auto, subset = train)
