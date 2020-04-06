# Rozdział 7 - Metoda wektorów nośnych (SVM)
rm(list=ls())

library(e1071)

#generate a random dataset
data_x <- runif(40, min=0, max=10)
data_y <- runif(40, min=0, max=10)

#plot the dataset
plot(data_x,data_y)

#create a non-linearly separable problem
class <- rep("A",40)
class[data_x>2.5 & data_x<4.5] <- "B"

#dataframe with x,y and labels
data <- data.frame(data_x,data_y,class)
data

#lets plot the dots
plot(data_x,data_y,col=ifelse(class=="A","red","blue"), pch=16)

#tune the model parameters: gamma and cost
tuned_model <- tune(svm,class~.,data=data,kernel="radial",ranges=list(cost=c(0.01,0.1,1,10), gamma=c(0.01,0.1,1,10)))

#get the best model
best_model <- tuned_model$best.model
best_model

plot(best_model,data)

#plot the best model
plot(best_model,data)

predict(best_model, data.frame(data_x=8,data_y=8))
