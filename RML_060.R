# Rozdział 6 - Naiwny klasufikator Bayesa

library(caret)
library(e1071)
library(class)

rm(list=ls())

#odczyt danych
iris_dataset <- iris

#losowanie danych
iris_dataset <- iris_dataset[sample.int(nrow(iris_dataset)),]

#kilka pierwszych przypadków
head(iris_dataset)

#wykres cech irysów
ggplot(iris_dataset, aes(x = Sepal.Width, y = Petal.Length)) + geom_point(aes(color = Species))

#przekodowaie danych
iris_dataset$Species <- factor(iris_dataset$Species, labels=c("A","B","C"))

#normalizacja min-max
normalize <- function(x) {return( (x-min(x)) / (max(x)-min(x)) )}
normalized_iris_dataset <- as.data.frame(lapply(iris_dataset[1:4], normalize))
normalized_iris_dataset$Species <- iris_dataset$Species

training_dataset <- normalized_iris_dataset[1:100,]
test_dataset <- normalized_iris_dataset[101:150,]

model <- naiveBayes(Species ~ . , data=training_dataset)

predictions <- predict(model, test_dataset)
actual_labels <- test_dataset[,5]

#macierz błedów
table(predictions,actual_labels)

#dokładność modelu
mean(actual_labels==predictions)
