# Rozdział 11 - Sieci neuronowe

rm(list=ls())
library(neuralnet)

#Definiujemy zbiór treningowy dla zadania XOR
training_input_x <- c(0,0,1,1)
training_input_y <- c(0,1,0,1)
training_output <-c(0,1,1,0)

#utworzenie ramki dla zbioru treningowego
data <- data.frame(training_input_x,training_input_y,training_output)

#utworzenie sieci neuronowej
#linear.output = TRUE dla regresji and FALSE problemu klasyfikacji
neural_network <-neuralnet(training_output~training_input_x+training_input_y, data, hidden=3, threshold = 0.001, algorithm = "backprop", learningrate = 0.1, linear.output = TRUE)

#wykres archtektury sieci euronowej
plot(neural_network)

#prognozowanie danych
prediction <- compute(neural_network, data.frame(training_input_x=1,training_input_y=1))
prediction$net.result

#zadanie dla danych kredytowych
credit_data <- read.csv("./Dane/06 - credit_data.csv",TRUE,",")

library(neuralnet)

#kilka pierwszych danyh
head(credit_data)

#normalizacja zbioru danych
normalize <- function(x) {return( (x-min(x)) / (max(x)-min(x)) )}
normalized_credit_data <- as.data.frame(lapply(credit_data[2:4], normalize))
#dodanie nazw kolumny
normalized_credit_data$default <- credit_data$default

#podział danych na zbiór treningowy i testowy
training_data <- normalized_credit_data[1:1500,]
test_data <- normalized_credit_data[1501:2000,]

#uczenie sieci neuronowej
neural_network <- neuralnet(default~income+age+loan, training_data, hidden=5, act.fct = "logistic", linear.output = FALSE)

#wykres architektury sieci
plot(neural_network)

#testowanie
test_data2 <- subset(test_data,select=c("income","age","loan"))

#prognozowanie
predictions <- compute(neural_network, test_data2)

#zaokrąglenie
predictions <- round(predictions$net.result)

#dokładność modelu
mean(test_data$default==predictions)

