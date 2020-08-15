# 7. In the chapter, we mentioned the use of correlation-based distance
# and Euclidean distance as dissimilarity measures for hierarchical clustering.
# It turns out that these two measures are almost equivalent: if
# each observation has been centered to have mean zero and standard
# deviation one, and if we let r_ij denote the correlation between the ith
# and jth observations, then the quantity 1 - r_ij is proportional to the
# squared Euclidean distance between the ith and jth observations.
# On the USArrests data, show that this proportionality holds.
# Hint: The Euclidean distance can be calculated using the dist() function,
# and correlations can be calculated using the cor() function.


library(ISLR)


x = USArrests
x['states'] = row.names(x)
y <- x[ , 1:4]

standardize <- function(dataframe_input){
  mean_values <- apply(dataframe_input, 2, mean)
  variance_values <- apply(dataframe_input, 2, var)
  std_values <- sqrt(variance_values)
  demeaned_dataframe <- sweep(dataframe_input, 2, mean_values)
  std_norm_dataframe <- sweep(demeaned_dataframe, 2, std_values, "/")
  return(std_norm_dataframe)
}

z <- standardize(y)



Euclidean_distances <- dist(y)
correlations <- cor(y)

centered_correlations <- cor(z)
centered_Euclidean_distances <- dist(z)


dim(Euclidean_distances)
length(Euclidean_distances)
dim(correlations)
dim(centered_correlations)
dim(centered_Euclidean_distances)
length(centered_Euclidean_distances)

# May need to visit the solutions manual to take me home.
#  I have the material, but I'm not sure how to get from a
#  4 x 4 matrix and compare it to the results in a 1225
#  length vector.