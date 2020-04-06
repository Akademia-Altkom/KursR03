# Regresja dla danych ilościowych ---------------------------------------
# Wczytanie i przygotowanie danych oraz zamiana na system metryczny -----

rm(list=ls())                               #usuwanie danych
library(dplyr)
Dane<-trees
Dane<-Dane %>% transmute('Średnica'=Girth*2.54,
                         'Wysokość'=Height*2.54*12/100,'Objętość'=Volume*(2.54*12/100)^3)

# Wyznaczanie modelu regresji liniowej i jego optymalizacja -------------

model<-lm(data = Dane,formula = Objętość~.) #regresja z uwzględnieniem wszystkich kolumn
summary(model)                              #parametry modelu regresji
anova(model)                                #analiza dokładności modelu (ANOVA)
print(RMSE<-sqrt(mean((Dane$Objętość-model$fitted.values)^2))) #średni bład kwadratow

step(model,direction = "both")              #metoda krokowa optymalizajci modelu


# Wizualizacja modelu regresji ------------------------------------------

library(ggplot2)
rownanie<-paste("y=Średnica*",round(model$coefficients[[2]],3),"+Wysokość*",
                round(model$coefficients[[3]],3),"+(" ,round(model$coefficients[[1]],3),")",sep = "")
#punkty na wykresie to Dane oryginalne,czerwona linia to Dane z modelu
#słupki błedów to 95% przedział istotności
ggplot(Dane,aes(y=Objętość,x=1:length(Dane[,1])))+geom_point()+
  xlab("")+geom_errorbar(ymin=predict(model,Dane,interval="confidence")[,2],
                         ymax=predict(model,Dane,interval="confidence")[,3],col="green")+
  geom_line(aes(y=model$fitted.values,color="red"))+
  ggtitle(label="Regresja liniowa",subtitle=rownanie)+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))+
  theme(legend.position = "")

# Prognoza dla nowych danych --------------------------------------------

predict(model,newdata = (data.frame(Wysokość=50,Średnica=25)),interval = "confidence")

# Analiza statystyczna opisowa pliku 02 - analiza płac.csv --------------

# Wczytanie i transformacja pliku csv 02 - analiza płac.csv -------------

rm(list=ls())
Dane<-read.table("./Dane/02 - analiza płac.csv",header = T,
                 dec=",",sep = ";",encoding = "UTF-8")
colnames(Dane)[1]<-"Płeć"
colnames(Dane)[5]<-"Płaca"
Dane<-Dane[complete.cases(Dane),]   #Wyrzucenie z danych niepełnych obserwacji NA

# Wyznaczenie modelu ----------------------------------------------------

model<-lm(data=Dane,formula=Płaca~.)
summary(model)

# Zmiana czynnika odniesienia -------------------------------------------

contrasts(Dane$Płeć) #wyświetlenie wartośći całkowitych przyporzadkowanych do czynników
Dane$Płeć<-relevel(Dane$Płeć,ref="Mężczyzna") #zmiana czynnika względem, 
Dane$Wykształcenie<-relevel(Dane$Wykształcenie,ref="Wyższe") #którego bedzie zbudowany model 

model<-lm(data=Dane,formula=Płaca~.)
summary(model)
anova(model)
print(RMSE<-sqrt(mean((Dane$Płaca-model$fitted.values)^2)))

# Prognoza dla nowych danych --------------------------------------------

nowe_Dane<-data.frame(Płeć=c("Kobieta","Mężczyzna"),Wykształcenie=c("Wyższe","Wyższe"),
                      Wiek=c(65,65),Staż.pracy=c(30,30))
print(prognoza<-predict(model,newdata = nowe_Dane,interval = "confidence"))
diff(prognoza)                              #różnica między wierszami
summary(model)$coefficients                 #czyli współczynnik PłećKobieta to różnica

# Ćwiczenia
#a) Sprawdzić, jak i czy skala nasilenia choroby jest zależna od pozostałych czynników (plik "03 - choroba.csv")
#b) Zbadać, od czego zależy nadciśnienie i w jakim stopniu te czynniki wpływają na na podwyższenie ciśnienia,
#a) Sprawdzić, jak nakłady środków na reklamę (radio, tv, prasa) wpływa na sprzedaż (plik "04 - reklama.csv")