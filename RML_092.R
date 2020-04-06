# Rozdział 9 - Segmentacja - ćwiczenia"

rm(list=ls())

library(stats)

x <- runif(100,0,10)

y <- runif(100,0,10)

plot(x,y)

#dataset (it is an unsupervised learning so no labels!!!)
data <- data.frame(x,y)

data

#k means clustering algorithm
#needs some initialization (the centroid positions at the beginning)
#clustering is sensitive to initial selection of centroids: nstart attempts
#multiple initial configurations and find the best one
model <- kmeans(data, centers=6, nstart=15)

#between_sum_of_squares/total_sum_of_squares
#percent of variance explained by the clusters (higher the better)
model

plot(data,col=model$cluster,pch=16)

