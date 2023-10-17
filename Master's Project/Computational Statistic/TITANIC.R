rm(list=ls())
library(rstudioapi)
library(DescTools)
library(corrplot)
library(psych)
library(openxlsx)
library(car)
library(DescTools)
library(corrplot)
library(psych)
library(tidyverse)
library(data.table)
library(gridExtra)
library(caret)
library(MASS)
library(mlbench)
library(class)
library(DataExplorer)
library(HH)
library(Metrics)
library(olsrr)
library(dplyr)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

data1<- read.csv("train1.csv", header=T, sep=",")
data1= data.frame(data1)# d
head(data1)
dim(data1)
str(data1)
introduce(data1)
duplicated(data1)
table(data1$Survived)
table(data1$Pclass)
data=drop_na(data1)
table(data$Survived)
table(data$Pclass) #molti dati dei passeggeri di 3 classe sono andati persi

data$Embarked=as.factor(data$Embarked)
data$Embarked=as.numeric(data$Embarked)

# Guardiamo le variabili
dev.new()
plot(data)

#tabella correlazioni
numeric.var <- sapply(data, is.numeric)
str(numeric.var)
C <- cor(data[,numeric.var])
dev.new()
corrplot(C, method = 'number')
corrplot(C, method = 'circle')

# regressione per capire su cosa focalizzarci
model11=lm(Survived~.-Name-Ticket-Cabin,data=data)
vif(model11)
summary(model11)
ols_test_normality(model11)
# plot dati risultati significativi dalla regressione
ggplot(data1, aes(x=Survived))+geom_bar(fill="darkturquoise")+labs(title= "Sopravvissuti vs Deceduti",y="quantità", x="   morti                                                     sopravvissuti")
      
ggplot(data, aes(x=Age))+
        geom_histogram(binwidth =2, fill= "coral2")+
          labs(y="Passeggeri",
               x="Età",
               title= "Distribuzione Età")
ggplot(data, aes(x=Pclass))+
  geom_histogram(binwidth = 0.5, fill="coral2")+
  labs(y="Passeggeri",
       x="Classe sociale",
       title= "Distribuzione Classe sociale")

datas=mutate(data, Survived= factor(Survived))


ggplot(datas, aes(x=Age, fill=Survived))+
  geom_histogram(binwidth = 2)+
  labs(y="Passengeri",
       x="Età",
       title= "Distribuzione sopravvissuti (1) morti (0) per età")

ggplot(datas, aes(x=Pclass, fill=Survived))+
  geom_histogram(binwidth = 0.5)+
  labs(y="Passeggeri",
       x="Classe sociale",
       title= "Sopravvissuti-Morti per classe sociale")
ggplot(datas, aes(x=Age, fill=Survived))+
    facet_wrap(Sex~Pclass)+
    geom_histogram(binwidth=2)+
      labs(y= "Passeggeri",
           x="Età",
           title= "Sopravvissuti per età, classe sociale e sesso")


model=glm(Survived~.-Name-Ticket-Cabin,  family=binomial(link="logit"), data=data)
vif(model)
summary(model)

# Nuovo dataset con solo variabili "usabili"
DBF=data[,-12][,-11][,-9][,-4]

d1=as.numeric(DBF$PassengerId)
d2=as.numeric(DBF$Age)
d3=as.numeric(DBF$Survived)
d4=as.numeric(DBF$Pclass)
d5=as.numeric(DBF$SibSp)
d6=as.numeric(DBF$Parch)
d7=as.numeric(DBF$Fare)
dd8=as.factor(DBF$Sex)
d8=as.numeric(dd8)
d9=data$Embarked
# fare i dataset in questo modo quando sai che dovrai costruirne altri con le stesse variabili
# secondo me risparmia tempo, basta bindare le variabili che ti interessano e hai un dataset

a=cbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)
str(a)

data_elabora=data.frame(a)

out_LR <-glm(d3~.,  family=binomial(link="logit") , data=data_elabora)#
yprob <-predict(out_LR,data_elabora,"response")
y_prev<- ifelse(yprob>0.5,'1','0')
y_oss <- data_elabora$d3 ;
n <- nrow(data_elabora)
T0 = sum((y_prev==0&y_oss==0))
T1 = sum((y_prev==1&y_oss==1)) 
F0 = sum((y_prev==0&y_oss==1)) 
F1 = sum((y_prev==1&y_oss==0))
accuracy=((T0+T1)/n)*100
specificita=(T1/(T1+F0))*100
sensitivita =(T0/(T0+F1))*100
accuracy    # indica quante volte prendi "il centro del bersaglio", la precisione invece quante
            # volte prendi nello stesso punto
specificita # raramente sbaglia classificazione morti / falsi positivi
sensitivita # raramente sbaglia classificazione vivi/ falsi negativi

out_LM <-lm(d3~., data=data_elabora)

g=ols_step_best_subset(out_LM)
g
k=ols_step_all_possible(out_LM)
k
vif(out_LM)


#modello senza PassengerID(d1) e Embarked(d9), consigliato da stepwise
b=cbind(d2,d3,d4,d5,d8)

data_elaborab=data.frame(b)

out_LRb <-glm(d3~.,  family=binomial(link="logit") , data=data_elaborab)
yprob <-predict(out_LRb,data_elaborab,"response")
y_prev<- ifelse(yprob>0.5,'1','0')
y_oss <- data_elaborab$d3 ;
n <- nrow(data_elaborab)
T0 = sum((y_prev==0&y_oss==0))
T1 = sum((y_prev==1&y_oss==1)) 
F0 = sum((y_prev==0&y_oss==1)) 
F1 = sum((y_prev==1&y_oss==0))
accuracy=((T0+T1)/n)*100
specificita=(T1/(T1+F0))*100
sensitivita =(T0/(T0+F1))*100 
accuracy
specificita
sensitivita
out_LRc <-lm(d3~., data=data_elaborab)
summary(out_LRc)

# Previsione senza PassengerID e Embarked
#ind <- sample(2, nrow(data_elaborab), replace=TRUE, prob=c(0.70, 0.30))
#ftable(ind)  Se vuole far girare questo commento inverta anche sotto in data.training e data.test
morti=subset(data_elaborab, d3== 0)
vivi=subset(data_elaborab, d3== 1)

morti7<-sample_frac(morti, 0.7)
kek<-as.numeric(rownames(morti7)) 
morti3<- morti[-kek,]

vivi7<-sample_frac(vivi, 0.7)
wew<-as.numeric(rownames(vivi7)) 
vivi3<- vivi[-wew,]

training=rbind(vivi7,morti7)
test=rbind(morti3,vivi3)
data.training <- training[,1:5]#data_elaborab[ind==1,1:5]#
data.test <- test[,1:5]#data_elaborab[ind==2,1:5]#

out_LA <-glm(d3 ~.,  family=binomial(link="logit"), data=data.test)
xpred=predict(out_LA,data.test,"response")
x_prev<- ifelse(xpred>0.5,'1','0')
x_oss <- data.test$d3
tab <- xtabs(~x_prev+x_oss)
tab
n <- nrow(data_elaborab)
T0 = sum((x_prev==0&x_oss==0)) 
T1 = sum((x_prev==1&x_oss==1)) 
F0 = sum((x_prev==0&x_oss==1))
F1 = sum((y_prev==1&y_oss==0))
accuracy=((T0+T1)/n)*100
specificita=(T1/(T1+F0))*100
sensitivita =(T0/(T0+F1))*100
accuracy
specificita
sensitivita
summary(out_LA)

# Cerchiamo di capire perchè l'accuratezza è così bassa
library(klaR)
dev.new()
z=lda(d3~., data= data_elaborab)
plot(z) # il plot sembra mostrare che i due gruppi presentino molte zone comuni
        # forse è per questo che non si riesce a prevedere adeguatamente il fenomeno
print(z)


#
qqnorm(data$Fare, pch = 1, frame = FALSE)
qqline(data$Fare, col = "blue", lwd = 2)
#
boxplot(Age~Pclass, horizontal=FALSE, data=data, 
        main="Età per Classe sociale", xlab="Pclass ",
        ylab="Età",notch=FALSE, col="yellow")
boxplot(SibSp~Survived, horizontal=FALSE, data=data, 
        main="Partner e Fratelli/Sorelle a bordo per sorpavvissuti", xlab="Sopravvissuti ",
        ylab="Partner/Fratelli/Sorelle",notch=FALSE, col="yellow")
boxplot(Parch~Survived, horizontal=FALSE, data=data, 
        main="Genitori e Figli per sorpavvissuti", xlab="Sopravvissuti",
        ylab="Genitori e Figli",notch=FALSE, col="yellow")
boxplot(Age~Survived, horizontal=FALSE, data=data, 
        main="Età per sopravvissuti", xlab="Sopravvissuti",
        ylab="Età",notch=FALSE, col="yellow")
boxplot(Fare~Survived, horizontal=FALSE, data=data, 
        main="Biglietto per sopravvissuti", xlab="Sopravvissuti",
        ylab="Tassa Biglietto",notch=FALSE, col="yellow") 
#