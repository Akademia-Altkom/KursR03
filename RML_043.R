# Rozdział 4 - regresja logistyczna

rm(list=ls())

challenger_dataset <- read.csv("./Dane/05 - challenger.csv",TRUE,",")

#print the dataset
head(challenger_dataset)
challenger_dataset

#we are after whether the o-ring has failed or not so 0/1 accordingly
challenger_dataset["Fail"] <- ifelse(challenger_dataset$Erosion<1,0,1)

#let's see the binary values: FAIL or not
challenger_dataset$Fail

#we can attach the dataset: we do not have to use the $ operator we can
#use the columns directly
attach(challenger_dataset)

#train the logistic regression model on the dataset
model <- glm(Fail~Temperature, challenger_dataset, family="binomial")
summary(model)

#plot the model
plot(Fail~Temperature)
lines(Temperature, model$fitted, type="l", col="red")

#predict the probability of fail when temperature is 53 fahrenheit
predict(model, data.frame(Temperature=53), type="response")

