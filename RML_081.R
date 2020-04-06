# Rozdział 8 - Drzewa decyzyjne
rm(list=ls())

library(C50)

#read the credit scoring dataset
credit_data <- read.csv("./Dane/06 - credit_data.csv",TRUE,",")

#attach the dataset
attach(credit_data)

#first few lines + number of samples
head(credit_data)
nrow(credit_data)

#features (age,income,loan) + target (default)
features <- credit_data[2:4]
targets <- credit_data[6]

#split the dataset into training and test set (75%+25%)
training_features <- features[1:1500,]
training_targets <- targets[1:1500,]
test_features <- features[1501:2000,]
test_targets <- targets[1501:2000,]

#let's construct the decision tree classifier
model <- C5.0(x=training_features,y=as.factor(training_targets))

#check the model
summary(model)
plot(model)

#let's make predictions
predictions <- predict(model,test_features)
predictions

#confusion matrix
table(predictions,test_targets)
#accuracy of the model
mean(test_targets==predictions)
