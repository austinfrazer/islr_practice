# 10. In this problem, you will generate simulated data,
#   and then perform PCA and K-means clustering on the data.


###  Ended up going directly to the solutions from
#  http://blog.princehonest.com/stat-learning/ch10/10.html
#   for this go around.

# (a) Generate a simulated data set with 20 observations in each
#  of three classes (i.e. 60 observations total), and 50 variables.
#     Hint: There are a number of functions in R that you can use to
#     generate data. One example is the rnorm() function; runif() is
#     another option. Be sure to add a mean shift to the observations
#     in each class so that there are three distinct classes.

# Generating random data (from sol:  (http://blog.princehonest.com/stat-learning/ch10/10.html))
x = matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)
x[1:20, 2] = 1
x[21:40, 1] = 2
x[21:40, 2] = 2
x[41:60, 1] = 1
# x[1:20, 2] = x[1:20, 2] + 1
# x[21:40, 1] = x[21:40, 1] + 2
# x[21:40, 2] = x[21:40, 2] + 2
# x[41:60, 1] = x[41:60, 1] + 1




# #  My way didn't seem to work :(
####  My method was in fact more of a scale shift than a mean shift.
###  Or, it was a strictly defined mean shift, but the book should
#### have been asking for something else.  Shifting all data points
####  of all of the variables in each class by 1, 10, and 100 would
####  not necessarily lead to any difference in vectors.  Hence, in this
####  version, there were points mixed from all the classes everywhere.
# # adding mean shift to each class
#  For class 1, shift = 1
#  For class 2, shift = 10
#  For class 3, shift = 100
# x
# x[1:20, ] = x[1:20, ] + 1
# x[21:40, ] = x[21:40, ] + 10
# x[41:60, ] = x[41:60, ] + 100

# # Checking result of mean shift applied to random data.
# print(x)


# (b) Perform PCA on the 60 observations and plot the first two principal
# component score vectors. Use a different color to indicate
# the observations in each of the three classes. If the three classes
# appear separated in this plot, then continue on to part (c). If
# not, then return to part (a) and modify the simulation so that
# there is greater separation between the three classes. Do not
# continue to part (c) until the three classes show at least some
# separation in the first two principal component score vectors.

pca_noscale = prcomp(x, scale = FALSE)
pca_scale = prcomp(x, scale = TRUE)

pca_noscale$x[, c(1,2)]
plot(pca_noscale$x[ , c(1, 2)], col = 1:3, xlab="Z1", ylab="Z2", pch = 19)


# (c) Perform K-means clustering of the observations with K = 3.
# How well do the clusters that you obtained in K-means clustering
# compare to the true class labels?
#   Hint: You can use the table() function in R to compare the true
# class labels to the class labels obtained by clustering. Be careful
# how you interpret the results: K-means clustering will arbitrarily
# number the clusters, so you cannot simply check whether the true
# class labels and clustering labels are the same.
k = 3
km.out = kmeans(x, k, nstart = 20)
plot(x,
     col = (km.out$cluster + 1), 
     main = paste0("K - Means Clustering Results with K = ", k),
     xlab = "",
     ylab = "",
     pch = 20,
     cex = 2)
table(km.out$cluster, c(rep(1, 20), rep(2, 20), rep(3, 20)))
km.out$cluster

# (d) Perform K-means clustering with K = 2. Describe your results.

k = 2
km.out = kmeans(x, k, nstart = 20)
plot(x,
     col = (km.out$cluster + 1), 
     main = paste0("K - Means Clustering Results with K = ", k),
     xlab = "",
     ylab = "",
     pch = 20,
     cex = 2)
table(km.out$cluster, c(rep(1, 20), rep(2, 20), rep(3, 20)))
km.out$cluster
sort(km.out$cluster)

# (e) Now perform K-means clustering with K = 4, and describe your
# results.

k = 4
km.out = kmeans(x, k, nstart = 20)
plot(x,
     col = (km.out$cluster + 1), 
     main = paste0("K - Means Clustering Results with K = ", k),
     xlab = "",
     ylab = "",
     pch = 20,
     cex = 2)
table(km.out$cluster, c(rep(1, 20), rep(2, 20), rep(3, 20)))
km.out$cluster
sort(km.out$cluster)

# (f) Now perform K-means clustering with K = 3 on the first two
# principal component score vectors, rather than on the raw data.
# That is, perform K-means clustering on the 60 × 2 matrix of
# which the first column is the first principal component score
# vector, and the second column is the second principal component
# score vector. Comment on the results.

pca.out <- pca_noscale
km.out = kmeans(pca.out$x[ ,1:2], 3, nstart=20)
table(km.out$cluster, c(rep(1, 20), rep(2, 20), rep(3, 20)))

# Just like the first c -- a perfect match.

# (g) Using the scale() function, perform K-means clustering with
# K = 3 on the data after scaling each variable to have standard
# deviation one. How do these results compare to those obtained
# in (b)? Explain.

km.out = kmeans(scale(x), 3, nstart=20)
sort(km.out$cluster)