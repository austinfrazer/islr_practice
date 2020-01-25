source("C:\\Users\\Austin\\Documents\\ISLR_Labs\\MyWork\\Euclidean_Distance.R")

# Data from problem 7a from Section 2.4
X = matrix(c(0,3,0,2,0,0,0,1,3,0,1,2,-1,0,1,1,1,1), nrow=6, byrow=TRUE)
Y = c("Red", "Red", "Red", "Green", "Green", "Red")

# 7a
Euclidean_Distance(X)

# 7b - K=1 nearest neighbor:  Green (point 5 is nearest to test point)
# 7c - K=3 nearest neighbor:  Red (point 5 = green, point 6 = red, point 2 = red; therefore, Red)
# 7d - If Bayes decision boundary is highly nonlinear, is best value for K large or small.  (Small)