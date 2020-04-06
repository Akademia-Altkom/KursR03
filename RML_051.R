# Rozdział 5 - Klasyfikacja K-najbliższych sąsiadów

library(caret)
library(ggplot2)
library(e1071)
library(class)

rm(list=ls())

#read the iris dataset
iris_dataset <- iris

#distribution of the output classes
table(iris_dataset$Species)

#statistics 
summary(iris_dataset[c("Petal.Length","Petal.Width")])

#first few lines
head(iris_dataset)

#we can plot the dataset
ggplot(iris_dataset, aes(x = Sepal.Width, y = Petal.Length)) + geom_point(aes(color = Species))

#use classes A,B,C instead of the actual names
iris_dataset$Species <- factor(iris_dataset$Species, labels=c("A","B","C"))

#min-max normalization
normalize <- function(x) {return( (x-min(x)) / (max(x)-min(x)) )}

normalized_iris_dataset <- as.data.frame(lapply(iris_dataset[1:4], normalize))
normalized_iris_dataset

#split the datase: training + test set
training_dataset <- normalized_iris_dataset[1:120,]
test_dataset <- normalized_iris_dataset[121:150,]
#5th column is the output class A,B or C
training_labels <- iris_dataset[1:120,5]
test_labels <- iris_dataset[121:150,5]

#returns the predictions for the test dataset
predictions <- knn(train=training_dataset, test=test_dataset, cl=training_labels, k=10)
predictions
test_labels
#confusion matrix
table(predictions,test_labels)
#accuracy of the model
mean(test_labels==predictions)
