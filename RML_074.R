# Rozdział 7 - Metoda wektorów nośnych (SVM)
rm(list=ls())

library(e1071)

letters_dataset <- read.csv("./Dane/08 - letters_data.csv",TRUE,",")
attach(letters_dataset)
str(letters_dataset)

train_dataset <- letters_dataset[1:16000,]
test_dataset <- letters_dataset[16001:20000,]

#tune the model parameters: gamma and cost
tuned_model <- tune(svm,lettr~.,data=train_dataset, kernel="radial", ranges=list(gamma = 2^(-1:1), cost = 2^(2:4)))

#get the best model
best_model <- tuned_model$best.model
best_model

predictions <- predict(best_model, test_dataset)

predictions
test_dataset$lettr
#confusion matrix
table(predictions,test_dataset$lettr)
#accuracy of the model
mean(test_dataset$lettr==predictions)
