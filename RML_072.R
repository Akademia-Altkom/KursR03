# Rozdział 7 - Metoda wektorów nośnych (SVM)
rm(list=ls())

library(e1071)

#generate a random dataset
data_x <- runif(20, min=0, max=10)
data_y <- runif(20, min=0, max=10)

#plot the dataset
plot(data_x,data_y)

#lets create the class labels (A and B)
class <- rep("A",20)

#label B if x<5
class[data_x<5] <- "B"

#plot the dataset (A class with red, B class with blue)
plot(data_x,data_y,col=ifelse(class=="A","red","blue"),pch=16)

#create a dataframe out of x,y and the labels
data <- data.frame(data_x,data_y,class)

#let's use support vector machine (tune parameters)
tuned_model <- tune(svm, class~., data=data, kernel="linear", ranges=list(cost=c(0.001,0.01,0.1,1,10,100)))

best_model <- tuned_model$best.model
best_model
#we can make predictions with the model
predict(best_model, data.frame(data_x=8,data_y=8))
