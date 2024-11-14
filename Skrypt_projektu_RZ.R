#Preparing data set
library(readxl)
Drukarki <- read_excel("Dane/Drukarki.xlsx")
View(Drukarki)


#Cleaning dataset, load libraries essential to preparing data for analysis 

library(dplyr)
library(ggplot2)

Drukarki %>% colnames()

#Select columns for analysis
head(Drukarki,10)
zbior<-Drukarki %>% select(c(2,3,16,4,12,8))
zbior<-as.data.frame(zbior)
rownames(zbior)<-Drukarki$`printer model`


colnames(zbior)
#Remove NA's records

zbior<-zbior %>%mutate(`average user/expert rating [1-5]`=as.numeric(`average user/expert rating [1-5]`)) %>% filter(complete.cases(.))
length(zbior$price)
#19 - wiekszosc rekordow 
#Minimalizujemy ceny i wysokość,  maksymalizujemy pozostałe zmienne. 

crit<-c("min","min","min",rep("max",3))
crit
##Fixing of variables
#Important predictors: prices, rating and speed. 
weights<-c(0.3, 0.25, 0.05,0.12,0.22,0.06)

##using TOPSIS method to find best printer according to criterias 

results<-TOPSIS(zbior, weights, crit)
results
max(results)
summary(results)
ramka<-results %>% as.data.frame()
ramka$nazwa<-rownames(ramka)
ramka$wynik<-ramka$.

ramka %>%arrange(desc(wynik))%>% head(5)%>% arrange(desc(wynik))%>% ggplot()+geom_col(aes(reorder(nazwa,-wynik),wynik), fill="ghostwhite")+xlab("Printer")+labs(title="TOPSIS result")+
theme_minimal()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#Using different weights  - technical parameters are more important
colnames(zbior)
weights<-c(0.15, 0.15, 0.05,0.3,0.22,0.13)
sum(weights)


results<-TOPSIS(zbior, weights, crit)
max(results)
colnames(zbior)
##still this same printer in TOPSIS

#Only technical parameters
colnames(zbior2)
zbior2<-zbior[,c(4:6)]
weigths2<-c(0.5,0.3,0.2)
crit2<-c(rep("max",3))

results2<-TOPSIS(zbior2, weigths2, crit2)
results2
