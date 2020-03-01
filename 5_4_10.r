# 4.7.10
#  The 'Weekly' data is inside the ISLR package.

library(ISLR)

# a  Do some numerical and graphical summaries of the data.  Do there appear to be any patterns?
names(Weekly)
dim(Weekly)

summary(Weekly)
weekly_directionless = subset(Weekly, select = -c(Direction))
weekly_directionless
cor(weekly_directionless)

# Not much in terms of correlations.

head(Weekly)
plot(Weekly$Volume)

#  The plot reveals that trading volume is a lot higher now than it was in the past.  It appears to have pe

subset(Weekly, Volume > 9)

subset(Weekly, Year == max(Year), select = c(Volume))

test_year = Weekly$Year

max_year = max(test_year)


# Will continue data exploration next time.
pairs(Weekly)


# b)  Use the full data set to perform a logistic regression with Direction
#     as the response and the five lag variable plus Volume as predictors
#     Use the summary function to print the results.  
#     Do any of the predictors appear to be statistically significant?
#     If so, which ones?

glm.weekly = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(glm.weekly)

# Only the intercept, which is meaningless, and Lag2 (p = .0296) are statistically significant.

# c Compute the confusion matrix and overall fraction of correct predictions.  Explain.
glm.probs = predict(glm.weekly, type = "response")
glm.probs[1:10]

contrasts(Weekly$Direction)


glm.pred = rep("Down", nrow(Weekly))
glm.pred[glm.probs > 0.5] = "Up"

table(glm.pred, Weekly$Direction)
mean(glm.pred == Weekly$Direction)  # This regression was accurate 56.1% of the time.  It looks like down is being vastly underpredicted.  
                                    # 484 times down happens; 605 times, up happens.  Yet, only 102 times is down predicted compared to 987 for up...

# d Now fit using training data period from 1990 - 2008 with lag2 as only predictor.
train = (Weekly$Year < 2009)
Weekly.2009 = Weekly[!train, ]
dim(Weekly.2009)
Direction.2009 = Weekly$Direction[!train]

glm.fits = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
glm.probs = predict(glm.fits, Weekly.2009, type = "response")

glm.pred = rep("Down", nrow(Weekly.2009))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2009)
mean(glm.pred == Direction.2009) #  .625 or 62.5%.  This is better than we had before.

# e)  Repeat (d) using LDA.
library(MASS)
lda.fit = lda(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
lda.fit

lda.pred = predict(lda.fit, Weekly.2009)
names(lda.pred)

lda.class = lda.pred$class
length(lda.class)
length(Weekly.2009)
dim(Weekly.2009)

table(lda.class, Direction.2009)

mean(lda.class == Direction.2009)   # 62.5% ; This is identical in accuracy to the logistic regression.  
                                    # The confusion matrix is identical as well.

# f  Repeat (d) using QDA

qda.fit = qda(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
qda.fit

qda.pred = predict(qda.fit, Weekly.2009)
names(qda.pred)

qda.class = qda.pred$class
length(qda.class)
length(Weekly.2009)
dim(Weekly.2009)

table(qda.class, Direction.2009)

mean(qda.class == Direction.2009)   # 58.7% ; This is lower in accuracy than the other two.  It always predicts "Up."

# g)  Repeat (d) using KNN with K = 1
library(class)
train.X = as.matrix((Weekly$Lag2)[train])
test.X = as.matrix((Weekly$Lag2)[!train])
train.Direction = Weekly$Direction[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2009)

output = table(knn.pred, Direction.2009)

(output[1, 1] + output[2, 2]) / sum(output) #  = 0.5  50%.

# h)  Which provides the best results?  Logistic == LDA.  Then QDA.  Then knn (k = 1)

# i)  Experiment with different combinations of predictors,
#             including possible transformations and interactions, for each of the methods
#             Report the variables, method, and associated confusion matrix that appears to
#             provide the best results on the held out data.  Note that you should also
#             experiment with values for k in the knn classifier.

knn.pred2 = knn(train.X, test.X, train.Direction, k=2)
knn.pred3 = knn(train.X, test.X, train.Direction, k=3)
knn.pred4 = knn(train.X, test.X, train.Direction, k=4)
knn.pred5 = knn(train.X, test.X, train.Direction, k=5)
knn.pred6 = knn(train.X, test.X, train.Direction, k=6)

table(knn.pred2, Direction.2009)
output = table(knn.pred2, Direction.2009)
(output[1, 1] + output[2, 2]) / sum(output)  # 51.9%

table(knn.pred3, Direction.2009)
output = table(knn.pred3, Direction.2009)
(output[1, 1] + output[2, 2]) / sum(output)  # 54.8%

table(knn.pred4, Direction.2009)
output = table(knn.pred4, Direction.2009)
(output[1, 1] + output[2, 2]) / sum(output)  # 59.6%  This is still less than LDA + Logit, but it is better than QDA

table(knn.pred5, Direction.2009)
output = table(knn.pred5, Direction.2009)
(output[1, 1] + output[2, 2]) / sum(output)  # 51.9%

table(knn.pred6, Direction.2009)
output = table(knn.pred6, Direction.2009)
(output[1, 1] + output[2, 2]) / sum(output)  # 53.8%

# ending exploration here.


