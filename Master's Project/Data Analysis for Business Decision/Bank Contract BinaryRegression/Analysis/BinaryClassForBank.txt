###librerie e dati ----
rm(list=ls())
library(rstudioapi)
library(readxl)
library(tidyr)
library(corrplot)
library(dplyr)
library(pscl)
library(DMwR)
library(MASS)
library(randomForest)
library(caret)
library(pROC)
library(lmtest)
###
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))
data1<- read_excel("data.xlsx",sheet = 2)
data2<- read_excel("data.xlsx",sheet = 3)
datoni <- merge(data1, data2, by = "Record ID")
#colnames(datoni)[1]<-"RecordID"
#datoniz <- subset(datoni, !duplicated(subset(datoni, select = -RecordID)))
datoni$Job<- as.factor(datoni$Job)
datoni$`Marital Status`<- as.factor(datoni$`Marital Status`)
datoni$Education<- as.factor(datoni$Education)
datoni$`Previous Default`<- as.factor(datoni$`Previous Default`)
datoni$`House Ownershi`<- as.factor(datoni$`House Ownershi`)
datoni$`Existing Loans`<- as.factor(datoni$`Existing Loans`)
datoni$`Contact Channel`<- as.factor(datoni$`Contact Channel`)
datoni$Month<- as.factor(datoni$Month)
datoni$`Day of Week`<- as.factor(datoni$`Day of Week`)
datoni$poutcome<- as.factor(datoni$poutcome)
datoni$pdays<- as.factor(datoni$pdays)
datoni$Target<- as.factor(datoni$Target)
colnames(datoni)[4]<-"MaritalStat"
colnames(datoni)[6]<-"PreviousDefault"
colnames(datoni)[7]<-"HouseOwner"
colnames(datoni)[8]<-"ExistingLoans"
colnames(datoni)[9]<-"ContactChannel"
colnames(datoni)[11]<-"Day"
colnames(datoni)[12]<-"CallDuration"
colnames(datoni)[21]<-"Target"
datoni$pdays = ifelse(datoni$pdays == "999",0,1)
#datoni$Age<- log(datoni$Age)
#datoni$campaign<-log(datoni$campaign)
#datoni<- datoni %>% filter(!(datoni$euribor3m>4))
#datonii<-datoni
#datoni<- datoni %>% filter(!(datoni$pdays==0 & datoni$previous>0))           



### train test----
datoni<-datoni[,-c(1,12,17,18,19)]
morti=subset(datoni, Target== 0)
vivi=subset(datoni, Target== 1)
morti7<-sample_frac(morti, 0.7)
kek<-as.numeric(rownames(morti7)) 
morti3<- morti[-kek,]
vivi7<-sample_frac(vivi, 0.7)
wew<-as.numeric(rownames(vivi7)) 
vivi3<- vivi[-wew,]
training=rbind(vivi7,morti7)
test=rbind(morti3,vivi3)


# datonii<-datonii[,-c(1,12,18,19)]
# mortii=subset(datonii, Target== 0)
# vivii=subset(datonii, Target== 1)
# mortii7<-sample_frac(mortii, 0.7)
# keki<-as.numeric(rownames(mortii7)) 
# mortii3<- mortii[-keki,]
# vivii7<-sample_frac(vivii, 0.7)
# wewi<-as.numeric(rownames(vivi7)) 
# vivii3<- vivii[-wewi,]
# trainiing=rbind(vivii7,mortii7)
# testi=rbind(mortii3,vivii3)

### random forest per variabili significative ----
rf_model <- randomForest(Target ~ ., data = training)
var_importance <- importance(rf_model)



### train test con significative ----
trainingsub <- dplyr::select(training,euribor3m,Month,pdays,campaign,Target)
trainingsub = SMOTE(Target ~ ., data = trainingsub, perc.over = 380, perc.under = 135)
summary(trainingsub$Target==1) 
testsub<-dplyr::select(test,euribor3m,Month,pdays,campaign,Target)
#testsub<-dplyr::select(testi,euribor3m,pdays,Month,Job,Target)

model <- glm(Target ~ ., data = trainingsub, family = binomial(link="logit"))
summary(model)

previsioni <- predict(model, testsub, type="response")
confusionMatrix(as.factor(ifelse(previsioni>0.4, 1,0)), testsub$Target, mode = "everything", positive="1")
table(testsub$Target==1)
table(round(previsioni))

roc_obj <- roc(testsub$Target, previsioni)
plot(roc_obj)
auc_val <- auc(roc_obj)
cat("AUC:", auc_val)

nuovi_dati <- data.frame(euribor3m = 2,753, Month = 'mar', pdays = 1, campaign=1)
previsione <- predict(model, newdata = nuovi_dati, type = "response")
prev= ifelse(previsione > 0.4,1,0)
prev

p=as.data.frame(previsioni)
round(p)
table(round(p))
#### test wald ----
model_intercept = glm(formula = Target ~ 1, family = binomial(link = logit), data = trainingsub)
wald_test <- waldtest(model, model_intercept)
wald_test


#### test media residui ----
residuals = residuals(model)
round(mean(residuals))


#### test likelihood ----
log_likelihood <- logLik(model)
log_likelihood


#### mcfadden ----
mf<- 1-logLik(model)/logLik(model_intercept)
mf


