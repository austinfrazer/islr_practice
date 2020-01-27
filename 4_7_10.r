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

# c