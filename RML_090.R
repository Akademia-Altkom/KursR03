# Rozdział 9 - Segmentacja

# Wczytanie i przygotowanie danych

rm(list=ls())
Dane<-read.csv("./Dane/04 - reklama.csv")
Dane<-Dane[,-1]
# dodanie wartości odstających
Dane<-rbind(Dane,c(170,100,100,200))


# Segmentacja danych metodą k-średnich

## Dobór ilości segmentów metodą łokcia

k<-8
wss<-sapply(1:k,function(k){kmeans(Dane, k)$tot.withinss})

#optymalny dobór to tam gdzie jest zagięcie wykresu - tzw łokieć
library(ggplot2)
ggplot()+geom_line(aes(x=1:8,y=wss))+xlab("Ilość segmentów")+
  ylab("Średnia z sumy kwadratów wewnątrz każdego segmentu")+
  geom_vline(xintercept = 3,col="red",linetype="dashed")+
  geom_text(aes(x=3,y=0,label="3"),hjust = "right")

## Dobór ilości segmentów metodą średniej sylwetki

library(cluster)
k<-8
sc<-sapply(2:k,function(k){summary(silhouette(
  kmeans(Dane, k)$cluster,
  dist(Dane,"euclidean")))$avg.width})

#optymalny dobór to tam gdzie wykres przyjmuje wartość max
ggplot()+geom_line(aes(x=2:8,y=sc))+xlab("Ilość segmentów")+
  ylab("Średnia współczynnika sylwetki")+
  geom_vline(xintercept = 2,col="red",linetype="dashed")+
  geom_text(aes(x=2,y=0,label="2"),hjust = "right")

plot(silhouette(kmeans(Dane, 2)$cluster,dist(Dane,"euclidean")),main="")

## Dobór ilości segmentów metodą statystyki luk

Gap<-clusGap(Dane,FUN = kmeans,K.max = 8,B=100,spaceH0 = "original",
             d.power = 2)
print(Gap,method="firstSEmax")
plot(Gap,main="")

# Podział na 3 segmenty 

podzial<-kmeans(Dane,3)
Dane<-cbind(Dane,as.factor(podzial$cluster))
colnames(Dane)[5]<-"Segment_kmeans"

# Segmentacja danych metodą k-median

library(Gmedian)
podzial<-kGmedian(Dane[,-5],3)
Dane<-cbind(Dane,as.factor(podzial$cluster))
colnames(Dane)[6]<-"Segment_kmedian"
levels(Dane$Segment_kmedian)<-c("3","2","1") #zmiana numeracji segmentów

# Segmentacja danych metodą PAM

podzial<-pam(Dane[,c(-5,-6)],3)
Dane<-cbind(Dane,as.factor(podzial$cluster))
colnames(Dane)[7]<-"Segment_pam"
levels(Dane$Segment_pam)<-c("3","1","2") #zmiana numeracji segmentów

# Wizualizacja segmentacji

library(ggalt)
ggplot() + geom_point(data=Dane,aes(x=TV,y=Sales))+labs(col = "Segmenty")+
  geom_encircle(data=Dane,aes(x=TV,y=Sales,col=Segment_kmeans),expand=0)+
  geom_encircle(data=Dane,aes(x=TV,y=Sales,col=Segment_kmedian),expand=0)+
  geom_encircle(data=Dane,aes(x=TV,y=Sales,col=Segment_pam),expand=0)

#bez wartości odstającej
ggplot() + geom_point(data=Dane[-201,],aes(x=TV,y=Sales))+
  geom_encircle(data=Dane[-201,],
                aes(x=TV,y=Sales,fill=Segment_kmeans,col="kmeans"),
                expand=0,alpha=0.1,size=5)+
  geom_encircle(data=Dane[-201,],
                aes(x=TV,y=Sales,fill=Segment_kmedian,col="kmedian"),
                expand=0,alpha=0.2,size=5)+
  geom_encircle(data=Dane[-201,],
                aes(x=TV,y=Sales,fill=Segment_pam,col="pam"),
                expand=0,alpha=0.3,size=5)+
  labs(fill = "Segmenty")+theme(legend.position = "top")+labs(col="Metody")+
  labs(title="Wizualizacja segmentacji bez uwzględnienie wartości odstającej")+
  theme(plot.title = element_text(hjust = 0.5))

# Segmentacja hierarchiczna
## Podzial funkcją hclust

podzial<-hclust(dist(Dane[,1:4]),method = "complete")
plot(podzial)
segmenty<-cutree(podzial,k=3)
Dane<-cbind(Dane,hclust=as.factor(segmenty))
# wizualizacja
ggplot() + geom_point(data=Dane,aes(x=TV,y=Sales,col=hclust))+
  geom_encircle(data=Dane,aes(x=TV,y=Sales,col=hclust),expand=0)

## Podzial funkcją agnes

podzial<-agnes(Dane[,1:4],method = "ward")
plot(podzial)
segmenty<-cutree(podzial,k=3)
Dane<-cbind(Dane,agnes=as.factor(segmenty))
# wizualizacja
ggplot() + geom_point(data=Dane,aes(x=TV,y=Sales,col=agnes))+
  geom_encircle(data=Dane,aes(x=TV,y=Sales,col=agnes),expand=0)

rm(list=ls())
library(dplyr)
library(tidyr)
library(readxl)
Dane<-read_xls("./Dane/10 - kredyty.xls")
Dane<- Dane %>% select(LIMIT_BAL,SEX,EDUCATION,MARRIAGE,AGE) %>% 
  mutate_at(c("SEX","EDUCATION","MARRIAGE"),as.factor)
levels(Dane$SEX)<-c("Mężczyzna","Kobieta")
levels(Dane$MARRIAGE)<-c("W związku","Rozwiedziony","Kawaler/Panna","Inny")
tbl_df(head(Dane))

## 2. Segmentacja k-średnich
DaneS<-Dane %>% select(LIMIT_BAL,AGE) %>% mutate_all(scale)

var<-sapply(1:10,function(x) mean(kmeans(DaneS,x)$withinss))
library(ggplot2)
ggplot()+geom_line(aes(x=1:10,y=var))

podzial<- Dane %>% select(LIMIT_BAL,AGE) %>% mutate_all(scale) %>%  kmeans(3)
Dane<-bind_cols(Dane,data.frame(SEGMENT=podzial$cluster))

Podsumowanie<-Dane %>% group_by(SEGMENT,cut(AGE,c(0,40,60,80)),cut(LIMIT_BAL,breaks=c(0,100000,1000000))) %>% summarise(ILOSC=n())
levels(Podsumowanie$`cut(AGE, c(0, 40, 60, 80))`)<-c("młody","średni","stary")  
colnames(Podsumowanie)[2]<-"Wiek"
colnames(Podsumowanie)[3]<-"Limit kredytowy"
levels(Podsumowanie$`Limit kredytowy`)<-c("Mały","Duży")
spread(Podsumowanie,Wiek,"ILOSC")