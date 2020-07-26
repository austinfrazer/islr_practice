# 7. In the chapter, we mentioned the use of correlation-based distance
# and Euclidean distance as dissimilarity measures for hierarchical clustering.
# It turns out that these two measures are almost equivalent: if
# each observation has been centered to have mean zero and standard
# deviation one, and if we let rij denote the correlation between the ith
# and jth observations, then the quantity 1???rij is proportional to the
# squared Euclidean distance between the ith and jth observations.
# On the USArrests data, show that this proportionality holds.
# Hint: The Euclidean distance can be calculated using the dist() function,
# and correlations can be calculated using the cor() function.


library(ISLR)


x = USArrests
x['states'] = row.names(x)

