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

#  Adding this the next day, after looking at the solutions
#    manual.
centered_Euclidean_distances_squared = centered_Euclidean_distances ^ 2
centered_correlations_of_transpose = cor(t(z))

#  This is just renaming things to look like the a, b
#  terminology from the solutions manual.
my_a = centered_Euclidean_distances_squared
my_b = as.dist(1- centered_correlations_of_transpose)
summary(my_b/my_a)

# May need to visit the solutions manual to take me home.
#  I have the material, but I'm not sure how to get from a
#  4 x 4 matrix and compare it to the results in a 1225
#  length vector.


# Visiting the solutions manual, I find:
#  

library(ISLR)

set.seed(1)
dsc = scale(USArrests)
#  It turns out that my "standardize()" function was doing the 
#   exact same thing as this built-in "scale()" function.  Neat!


a = dist(dsc)^2
b = as.dist(1 - cor(t(dsc)))
summary(b/a)


# Found a different solution here:  
#  https://waxworksmath.com/Authors/G_M/James/Code/Chapter10/chap_10_prob_7.R
## Scale each observation (not the features):
#####  I think that this solution better answers the initial question.
USA_scaled = t(scale(t(USArrests)))

# The correlation of each sample with the other samples:
# 
Rij = cor(t(USA_scaled)) # -1 <= Rij <= +1 
OneMinusRij = 1 - Rij # 0 <= 1-Rij <= +2 
X = OneMinusRij[lower.tri(OneMinusRij)]

D = as.matrix( dist( USA_scaled )^2 )
Y = D[lower.tri(D)]

plot( X, Y )

summary( X/Y )