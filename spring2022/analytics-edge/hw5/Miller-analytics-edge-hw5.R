#-----------------------
# ANALYTICS EDGE
# HW5
# TIM MILLER
#-----------------------

## IMPORTS
library(flexclust)
library(ggcorrplot)


##----- PROBLEM 1a -----##
data <- read.csv("autoSurvey.csv")

# sum data set to get importance of columns
colSums(data[,-11, -12])

#calculate gender breakdown
percent_female = sum(data$gender) / nrow(data)
percent_male = 1 - percent_female

percent_male #0.9382093
percent_female #0.06179067

#calculate household breakdown
percent_3people = sum(data$household) / nrow(data)
percent_1_2people = 1 - percent_3people

percent_3people #0.591425
percent_1_2people #0.408575

##----- PROBLEM 1b -----##

correlation = cor(data)
ggcorrplot(correlation)

##----- PROBLEM 1d -----##

# Compute all-pair euclidian distances between observations
d <- dist(data)    # method = "euclidean"
class(d)

# Create the Hierarchical clustering
hclust.mod <- hclust(d, method="ward.D2")

# Plot the dendrogram
plot(hclust.mod, labels=F, ylab="Dissimilarity", xlab = "", sub = "")

## -- 2 clusters -- ##

# Cut down to 2 clusters and extract assignments
assignments <- cutree(hclust.mod, 2)
table(assignments)

# chart data to analyze more easily
clusterMeans = sapply(split(data, assignments), colMeans)
heatmap(clusterMeans, scale="none", main="Unscaled scores")
heatmap(clusterMeans, scale="row", 
        main="Which cluster scores highest on each variable?")
heatmap(clusterMeans, scale="col",
        main="Which variables score highest in each cluster?")

# print clusterMeans table to analyze
clusterMeans
## -- 2 clusters -- ##

# Cut down to 2 clusters and extract assignments
assignments <- cutree(hclust.mod, 2)
table(assignments)

# chart data to analyze more easily
clusterMeans = sapply(split(data, assignments), colMeans)
heatmap(clusterMeans, scale="none", main="Unscaled scores")
heatmap(clusterMeans, scale="row", 
        main="Which cluster scores highest on each variable?")
heatmap(clusterMeans, scale="col",
        main="Which variables score highest in each cluster?")

# print clusterMeans table to analyze
clusterMeans


## -- 3 clusters -- ##

# Cut down to 3 clusters and extract assignments
assignments <- cutree(hclust.mod, 3)
table(assignments)

# chart data to analyze more easily
clusterMeans = sapply(split(data, assignments), colMeans)
heatmap(clusterMeans, scale="none", main="Unscaled scores")
heatmap(clusterMeans, scale="row", 
        main="Which cluster scores highest on each variable?")
heatmap(clusterMeans, scale="col",
        main="Which variables score highest in each cluster?")

# print clusterMeans table to analyze
clusterMeans


# Varying the number of clusters
dat.hc.car <- data.frame(nclust = seq_along(hclust.mod$height),
                             dissimilarity = rev(hclust.mod$height))

# Plotting the results
print(ggplot(dat.hc.car, aes(x=nclust, y=dissimilarity)) +
        geom_line(lwd=2) +
        theme_bw() +
        xlab("Number of Clusters") +
        ylab("Dissimilarity") +
        xlim(0, 100) +
        theme(axis.title=element_text(size=18), axis.text=element_text(size=18)))


##----- PROBLEM 1g -----##

## -- 14 clusters -- ##

# Cut down to 14 clusters and extract assignments
assignments <- cutree(hclust.mod, 14)
table(assignments)

# chart data to analyze more easily
clusterMeans = sapply(split(data, assignments), colMeans)
heatmap(clusterMeans, scale="none", main="Unscaled scores")
heatmap(clusterMeans, scale="row", 
        main="Which cluster scores highest on each variable?")
heatmap(clusterMeans, scale="col",
        main="Which variables score highest in each cluster?")

# print clusterMeans table to analyze
clusterMeans


##----- PROBLEM 1h -----##

# kmeans
NumberOfClusters = 14
set.seed(2407)
km = kmeans(data, iter.max=100, NumberOfClusters)

# the number of observations in each cluster
km.size <- km$size
km.size



table(kmeans.mod)


clusterMeans = sapply(split(data, assignments), colMeans)




