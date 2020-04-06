# Rozdział 10 - Analiza składowych głównych PCA
## Analiza składowych głównych PCA danych USArrests

# Wczytanie i transformacja danych _USArrests_.

rm(list=ls())
library(dplyr)
library(tidyr)
Dane<-USArrests
colnames(Dane)<-c("Morderstwa","Zabójstwa","Populacja miejska[%]","Gwałty")
Dane<-Dane[,c(1,2,4,3)]
Dane %>% head() 

# Wstępna analiza
apply(Dane,2,function(x) c(summary(x),Sd=sd(x),Var=var(x)))
cor(Dane)

# Analiza  PCA

library(devtools)
library(tibble)
install_github("vqv/ggbiplot")

PCA_model <- prcomp(Dane,scale. = T,center = T)
summary(PCA_model)
screeplot(PCA_model,type="lines")
PCA_model
cat("Średnie dla zmiennych przed normalizacją",PCA_model$center)
cat("Odchylania std dla zmiennych przed normalizacją",PCA_model$scale)
PCA_model$x %>% data.frame() %>% rownames_to_column() %>% head()
library(ggbiplot)
Dane.pop<-as.factor(ifelse(Dane$`Populacja miejska[%]`>60,"Powyżej 60%","Poniżej 60%"))
ggbiplot(PCA_model,ellipse = TRUE, circle = TRUE,labels=rownames(Dane),groups = Dane.pop) + theme(legend.position = "top")

#Z wykresu osypiska wynika, że zmienne mozną rzutować na dwie płaszczyzny 
#- składową PC1 i PC2. Pierwsza z nich jest najbardziej skorelowana z wszystkimi 
#rodzajami przestępstw (odwrotnie proporcjonalna), a druga opisuje stopień 
#urbanizacji stanu. Zależy od procent populacji stanu zamiaszkałej w obszarach 
#miejskich (odwrotnie proporcjonalna).
