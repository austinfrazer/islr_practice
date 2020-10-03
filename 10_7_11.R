# # 11. On the book website, www.StatLearning.com, there is a gene expression
# # data set (Ch10Ex11.csv) that consists of 40 tissue samples with
# # measurements on 1,000 genes. The first 20 samples are from healthy
# # patients, while the second 20 are from a diseased group.

# # (a) Load in the data using read.csv(). You will need to select
# # header=F.

# input_filepath = file.choose()
### For silver laptop, 
input_filepath = "C:\\Users\\Austin\\Documents\\ISLR_Labs\\Ch10Ex11.csv"
x = read.csv(file = input_filepath, header = FALSE)

# # (b) Apply hierarchical clustering to the samples using correlation-based
# # distance, and plot the dendrogram. Do the genes separate
# # the samples into the two groups? Do your results depend on the
# # type of linkage used?

##  Tried it my way.  Didn't do the distance right.
hc.complete = hclust(dist(x), method = "complete")
hc.average = hclust(dist(x), method = "average")
hc.single = hclust(dist(x), method = "single")
hc.centroid = hclust(dist(x), method = "centroid")

par(mfrow = c(1, 1))
plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = .9)
plot(hc.average, main = "Average Linkage", xlab = "", sub = "", cex = .9)
plot(hc.single, main = "Single Linkage", xlab = "", sub = "", cex = .9)
plot(hc.centroid, main = "Centroid Linkage", xlab = "", sub = "", cex = .9)


##  Tried it the princehonest way.
dd = as.dist(1 - cor(x))
par(mfrow = c(1, 3))
plot(hclust(dd, method="complete"), main = "Complete Linkage")
plot(hclust(dd, method="average"), main = "Average Linkage")
plot(hclust(dd, method="single"), main = "Single Linkage")
# plot(hclust(dd, method = "centroid"), main = "Centroid Linkage")

# Dependent upon where you cut, you can get two or three reasonable
#  Clusters from the "complete" or "average" linkage method.
#   Contrary to what the princehonest solution says, you can't 
#   get two or three reasonable clusters from the Single Leakage
#   method for this data.
###  (Added and then ommented out Centroid linkage one.) 

# #   (c) Your collaborator wants to know which genes differ the most
# # across the two groups. Suggest a way to answer this question,
# # and apply it here.

#### just taking this directly from the princehonest solution.
data <- x
pr.out = prcomp(t(data))
summary(pr.out)

total_load = apply(pr.out$rotation, 1, sum)
indices = order(abs(total_load), decreasing=T)
indices[1:10]

### I think we want the "indices" and not the "total_load[indices]"
####  to be the final line here.

# total_load[indices[1:10]]