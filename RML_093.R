# Rozdział 9 - Segmentacja - ćwiczenia"

rm(list=ls())
library(stats)

x <- runif(50,0,10)

y <- runif(50,0,10)

plot(x,y)

#dataset (it is an unsupervised learning so no labels!!!)
data <- data.frame(x,y)

data

#hierarchical clustering
model <- hclust(dist(x), method="complete")

#plot the dendogram
plot(model)

#we want to end up with 3 clusters
cut <- cutree(model,3)
cut
