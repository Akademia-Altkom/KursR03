# Rozdział 8 - Drzewa decyzyjne
rm(list=ls())

library(tree)

#read the bank dataset
bank_data <- read.csv("./Dane/09 - bank.csv",TRUE,";")

#attach the dataset
attach(bank_data)

#first few lines + number of samples
head(bank_data)
nrow(bank_data)

#first 3000 smaples are for training
training_set <- sample(1:nrow(bank_data),3000)
training_set

#let's construct the decision tree classifier
model <- tree(y~., bank_data, subset = training_set)

#check the model
summary(model)
plot(model)
text(model, pretty=5)

#let's make predictions
predictions <- predict(model,bank_data[-training_set,], type="class")
predictions

#accuracy of the model
mean(bank_data[-training_set,17]==predictions)
