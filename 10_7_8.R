# In Section 10.2.3, a formula for calculating PVE was given in Equation
# 10.8. We also saw that the PVE can be obtained using the sdev
# output of the prcomp() function.
# On the USArrests data, calculate PVE in two ways:

library(ISLR)
my_data = USArrests

#   (a) Using the sdev output of the prcomp() function, as was done in
#       Section 10.2.3.

prcomp_sdev <- prcomp(my_data, center = T, scale = T)$sdev
pr_var <- prcomp_sdev ^2
pve_a <- pr_var/sum(pr_var)
pve_a

#   (b) By applying Equation 10.8 directly. That is, use the prcomp()
# function to compute the principal component loadings. Then,
# use those loadings in Equation 10.8 to obtain the PVE.
# These two approaches should give the same results.

phi <- prcomp(my_data)$rotation
num_var = dim(phi)[1]  # j
num_pc <- dim(phi)[2]  # m
m <- num_pc

x <- standardize(my_data)
n <- nrow(x)
p <- ncol(x)

denominator = 0
for (j in 1:p){
  for (i in 1:n){
    xij_sq <- x[i, j]^2
    denominator = denominator + xij_sq
  }
}

denominator

outer_sum <- 0
inner_sum <- 0
for (i in 1:n){
  outer_sum <- outer_sum + (inner_sum ^ 2)
  inner_sum <- 0
  for (j in 1:p){
    phijm_times_xij <- phi[j, 1] * x[i, j]
    inner_sum <- inner_sum + phijm_times_xij
  }
}

outer_sum
outer_sum/denominator


denominator <- matrix(NA, nrow = 1, ncol = m)  
outer_sum <- matrix(NA, nrow = 1, ncol = m)  
pve <- matrix(NA, nrow = 1, ncol = m)
for(k in 1:m){
  denominator[1, k] = 0
  for (j in 1:p){
    for (i in 1:n){
      xij_sq <- x[i, j]^2
      denominator[1, k] = denominator[1, k] + xij_sq
    }
  }

  outer_sum[1, k] <- 0
  inner_sum <- 0
  for (i in 1:n){
    outer_sum[1, k] <- outer_sum[1, k] + (inner_sum ^ 2)
    for (j in 1:p){
      phijm_times_xij <- phi[j, k] * x[i, j]
      inner_sum <- inner_sum + phijm_times_xij
    }
    pve[1, k] <- outer_sum[1, k] / denominator[1, k]
  }
}

pve_b <- pve

#  I couldn't get my part b to match my part a.
####  Taking this from an online solution 
# (http://blog.princehonest.com/stat-learning/ch10/8.html)
#  Solution A
pr.out = prcomp(USArrests, center=T, scale=T)
pr.var = pr.out$sdev^2
pve = pr.var / sum(pr.var)
pve

# Solution B
loadings = pr.out$rotation
pve2 = rep(NA, 4)
dmean = apply(USArrests, 2, mean)
dsdev = sqrt(apply(USArrests, 2, var))
dsc = sweep(USArrests, MARGIN=2, dmean, "-")
dsc = sweep(dsc, MARGIN=2, dsdev, "/")
for (i in 1:4) {
  proto_x = sweep(dsc, MARGIN=2, loadings[,i], "*")
  pc_x = apply(proto_x, 1, sum)
  pve2[i] = sum(pc_x^2)
}
pve2 = pve2/sum(dsc^2)
pve2
  
# Hint: You will only obtain the same results in (a) and (b) if the same
# data is used in both cases. For instance, if in (a) you performed
# prcomp() using centered and scaled variables, then you must center
# and scale the variables before applying Equation 10.3 in (b).

#  I'll retry my B solution with standardized variables.

standardize <- function(dataframe_input){
  mean_values <- apply(dataframe_input, 2, mean)
  variance_values <- apply(dataframe_input, 2, var)
  std_values <- sqrt(variance_values)
  demeaned_dataframe <- sweep(dataframe_input, 2, mean_values)
  std_norm_dataframe <- sweep(demeaned_dataframe, 2, std_values, "/")
  return(std_norm_dataframe)
}

phi <- prcomp(my_data)$rotation
num_var = dim(phi)[1]  # j
num_pc <- dim(phi)[2]  # m
m <- num_pc

x <- standardize(my_data)
n <- nrow(x)
p <- ncol(x)

denominator <- matrix(NA, nrow = 1, ncol = m)  
outer_sum <- matrix(NA, nrow = 1, ncol = m)  
pve <- matrix(NA, nrow = 1, ncol = m)
for(k in 1:m){
  denominator[1, k] = 0
  for (j in 1:p){
    for (i in 1:n){
      xij_sq <- x[i, j]^2
      denominator[1, k] = denominator[1, k] + xij_sq
    }
  }
  
  outer_sum[1, k] <- 0
  inner_sum <- 0
  for (i in 1:n){
    outer_sum[1, k] <- outer_sum[1, k] + inner_sum
    for (j in 1:p){
      phijm_times_xij <- phi[j, k] * x[i, j]
      inner_sum <- inner_sum + phijm_times_xij
    }
    pve[1, k] <- outer_sum[1, k] / denominator[1, k]
  }
}

pve_b <- pve

# My solution (b) still didn't work correctly.  Going to move on.
#  If I do this one again, I should start from scratch instead of
#  trying to pick up the pieces from this attempt.
