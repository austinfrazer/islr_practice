# 9. Consider the USArrests data. We will now perform hierarchical clustering
# on the states.

library(ISLR)
my_data = USArrests

# (a) Using hierarchical clustering with complete linkage and
# Euclidean distance, cluster the states.

hc_complete = hclust(dist(my_data), method = "complete")


# (b) Cut the dendrogram at a height that results in three distinct
# clusters. Which states belong to which clusters?

plot(hc_complete, main = "Complete Linkage", xlab = "", sub = "", cex = 0.9)
three_clusters_hierarchical_complete = cutree(hc_complete, 3)

# For easier viewing (quick)
sort(three_clusters_hierarchical_complete)


# For easier viewing (computationally and code-length inefficient, and, in the end, still not that pretty.)
in_1_num = 0
in_2_num = 0
in_3_num = 0
for (i in 1:length(three_clusters_hierarchical_complete)){
  if (three_clusters_hierarchical_complete[i] == 1){
    in_1_num = in_1_num + 1
  }
  else if (three_clusters_hierarchical_complete[i] == 2){
    in_2_num = in_2_num + 1
  }
  else if (three_clusters_hierarchical_complete[i] == 3){
    in_3_num = in_3_num + 1
  }
}

in_1_list = vector(mode = "list", length = in_1_num)
in_2_list = vector(mode = "list", length = in_2_num)
in_3_list = vector(mode = "list", length = in_3_num)

in_1_pos  = 1
in_2_pos  = 1
in_3_pos  = 1
  
for (i in 1:length(three_clusters_hierarchical_complete)){
  if (three_clusters_hierarchical_complete[i] == 1){
    in_1_list[in_1_pos] = names(three_clusters_hierarchical_complete[i])
    in_1_pos = in_1_pos + 1
  }
  else if (three_clusters_hierarchical_complete[i] == 2){
    in_2_list[in_2_pos] = names(three_clusters_hierarchical_complete[i])
    in_2_pos = in_2_pos + 1
  }
  else if (three_clusters_hierarchical_complete[i] == 3){
    in_3_list[in_3_pos] = names(three_clusters_hierarchical_complete[i])
    in_3_pos = in_3_pos + 1
  }
}

print("States in group 1 include:  ")
for (item in in_1_list){
  print(item)
}

print("States in group 2 include:  ")
for (item in in_2_list){
  print(item)
}

print("States in group 3 include:  ")
for (item in in_3_list){
  print(item)
}


# (c) Hierarchically cluster the states using complete linkage and Euclidean
# distance, after scaling the variables to have standard deviation
# one.

# checking mean and variance of initial data
apply(my_data, 2, mean)
apply(my_data, 2, var)
variance(my_data)

# Scaling data
scaled_my_data = scale(my_data)

# checking mean and variance of scaled data
apply(scaled_my_data, 2, mean)
apply(scaled_my_data, 2, var)

# Clustering the scaled data
scaled_hc_complete = hclust(dist(scaled_my_data), method = "complete")

# (d) What effect does scaling the variables have on the hierarchical
# clustering obtained? In your opinion, should the variables be
# scaled before the inter-observation dissimilarities are computed?
#   Provide a justification for your answer.

plot(scaled_hc_complete, main = "Complete Linkage, Scaled", xlab = "", sub = "", cex = 0.9)

# The tree looks better after scaling.  The data should be standardized because UrbanPop has
#   different units from the other variables.
