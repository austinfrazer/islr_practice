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