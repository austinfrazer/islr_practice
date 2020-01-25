# Chapter 2, Problem 8.

# a)  use read.csv() to read the data into R.
input_filepath = file.choose()

college = read.csv(file = input_filepath)

# b)  Look at the data using the fix() function

fix(college)
rownames(college) = college[ ,1]
fix(college)

college = college[ , -1]
fix(college)

# c)
## i.  produce a numerical summary
summary(college)

## ii.  Use the pairs()function to produce a scatterplot of the first 10 variables.
pairs(college[ , 1:10])

## iii.  Use the plot() function to produce side-by-side boxplots of Outstate versus Private.
plot(college$Outstate ~ college$Private)

#####  Bonus, just curious which colleges were most expensive circa 1995.
subset(college, Outstate > 20000)

## iv.  Create a new qualitative variable, called Elite, by binning the Top10perc variable.
Elite = rep("No", nrow(college))  # Sets default to no.
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)

summary(Elite)  # There are 78 elite universities, and 699 that are not elite.
plot(college$Outstate ~ college$Elite) # Side by side boxplots

## v.  Use hist() function to produce histograms...

hist(college$Top10perc)
hist(college$Outstate)
hist(college$Room.Board)
hist(college$Expend)

par(mfrow = c(2, 2))


## vi.  Continue Exploring the data, and provide a brief summary of what you discover.
  #  Wayne State takes everybody.
  #  MIT is underapplied to, relative to Harvard.
  #  Princeton is most selective in this data set, with Harvard being a close #2.
?hist


