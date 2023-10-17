#####librerie------
rm(list = ls())
library(readxl)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(corrplot)
library(dplyr)
library(MASS)
library(lmtest)
library(olsrr)#stepwise
library(caret)
library(Metrics)
library(rstudioapi)
## importiamo---- 
current_path <- getActiveDocumentContext()$path
data1 <- read_excel("data.xlsx", sheet=3)
data2<- read_excel("data.xlsx", sheet=2)
dim(data1)
str(data1)
head(data1)
summary(data1)
data1=drop_na(data1) 
dim(data2)
str(data2)
head(data2)
summary(data2)
data2=drop_na(data2) 
datoni <- merge(data1, data2, by = "Record ID")
datonzi <- datoni[!duplicated(datoni$Isin), ]

datoni<- datonzi[,-c(1,2,3,4)]
datoni<- datoni %>% filter(!(datoni$NET_SALES<=0)) 
datoni<- datoni %>% filter(!(datoni$ENTERPRISE_VALUE<=0)) 
#datoni <- datoni[datoni$EBITDA >= 0 & datoni$`RETURN_ON_ EQUITY` >= 0 | datoni$EBITDA < 0 & datoni$`RETURN_ON_ EQUITY` < 0,]

## correlazioni----
numeric.var <- sapply(datoni, is.numeric)
str(numeric.var)
C <- cor(datoni[,numeric.var])
#dev.new()
corrplot(C, method = 'number')
corrplot(C, method = 'circle')


#######pulisco variabili tolgo collinearità----
datoni$SUB_INDUSTRY<-as.factor(datoni$SUB_INDUSTRY)
celentano<- datoni[,-c(2,4,5,6,7)]
colnames(celentano)[4]<-"EPS"
#celentano <- celentano[celentano$EBITDA >= 0 & celentano$EPS >= 0 | celentano$EBITDA < 0 & celentano$EPS < 0,]

celentano <- celentano %>% filter(!(EPS>15)) 
celentano$EPS<- -(celentano$EPS)
celentano <- celentano %>% filter(!(EPS>15)) 
celentano$EPS<- -(celentano$EPS)
celentano <- celentano %>% filter(!(EBITDA>12.064000))
celentano <- celentano %>% filter(!(RETURN_ON_ASSET>25))
celentano$RETURN_ON_ASSET<- -(celentano$RETURN_ON_ASSET)
celentano <- celentano %>% filter(!(RETURN_ON_ASSET>45))
celentano$RETURN_ON_ASSET<- -(celentano$RETURN_ON_ASSET)
#write.xlsx(celentano, file = "C:/Users/Marco/Downloads/Celentano.xlsx", sheetName = "Nome_foglio", rowNames = FALSE)

###########outlier ebitda-----
IQR_EBITDA <- IQR(celentano$EBITDA)

# Calcola Q1 e Q3
Q1_EBITDA <- quantile(celentano$EBITDA, probs = 0.25)
Q3_EBITDA <- quantile(celentano$EBITDA, probs = 0.75)


# Calcola i limiti inferiore e superiore per gli outlier
lower_limit_EBITDA <- Q1_EBITDA - 1.5 * IQR_EBITDA
upper_limit_EBITDA <- Q3_EBITDA + 1.5 * IQR_EBITDA

# Identifica gli outlier
#outliers_EBITDA <- celentano[celentano$EBITDA < lower_limit_EBITDA | celentano$EBITDA > upper_limit_EBITDA,]
#sum(outliers_EBITDA)
celentano_no_outliers <- celentano[celentano$EBITDA >= lower_limit_EBITDA & celentano$EBITDA <= upper_limit_EBITDA, ]


########outliers ASSET-----
# Calcola l'IQR
IQR_RETURN_ON_ASSET <- IQR(celentano_no_outliers$RETURN_ON_ASSET)

# Calcola Q1 e Q3
Q1_RETURN_ON_ASSET <- quantile(celentano_no_outliers$RETURN_ON_ASSET, probs = 0.25)
Q3_RETURN_ON_ASSET <- quantile(celentano_no_outliers$RETURN_ON_ASSET, probs = 0.75)


# Calcola i limiti inferiore e superiore per gli outlier
lower_limit_RETURN_ON_ASSET <- Q1_RETURN_ON_ASSET - 1.5 * IQR_RETURN_ON_ASSET
upper_limit_RETURN_ON_ASSET <- Q3_RETURN_ON_ASSET + 1.5 * IQR_RETURN_ON_ASSET

# toglo gli outlier
celentano_no_outliers <- celentano_no_outliers[celentano_no_outliers$RETURN_ON_ASSET >= lower_limit_RETURN_ON_ASSET & celentano_no_outliers$RETURN_ON_ASSET <= upper_limit_RETURN_ON_ASSET, ]

#######Outliers EPS----
IQR_EPS <- IQR(celentano_no_outliers$EPS)

# Calcola Q1 e Q3
Q1_EPS <- quantile(celentano_no_outliers$EPS, probs = 0.25)
Q3_EPS <- quantile(celentano_no_outliers$EPS, probs = 0.75)


# Calcola i limiti inferiore e superiore per gli outlier
lower_limit_EPS <- Q1_EPS - 1.5 * IQR_EPS
upper_limit_EPS <- Q3_EPS + 1.5 * IQR_EPS

# toglo gli outlier
celentano_no_outliers <- celentano_no_outliers[celentano_no_outliers$EPS >= lower_limit_EPS & celentano_no_outliers$EPS <= upper_limit_EPS, ]
celentano<-celentano_no_outliers
 
#celentano<- as.data.frame(scale(celentano))
######regressione----
reg_lineare <- lm(EPS ~ EBITDA+RETURN_ON_ASSET+SUB_INDUSTRY, data= celentano)
summary(reg_lineare)
dev.new()
plot(celentano$`EBITDA`, celentano$EPS)
abline(reg_lineare)
bptest(reg_lineare)
library(DAAG)
#dev.new()
#cv<-cv.lm(data=celentano, form.lm=formula(reg_lineare),m=5)###usa rlm da app
#mean(cv$cvpred)
#predict(reg_lineare, newdata = celentano)
#########
scaler <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# scala le variabili EBITDA e RETURN_ON_ASSET
celentano$EBITDA<- scaler(celentano$EBITDA)
celentano$RETURN_ON_ASSET <- scaler(celentano$RETURN_ON_ASSET)
celentano$EPS <- scaler(celentano$EPS)

########cv
library(caret)
set.seed(123)
nrif=5
controllo <- trainControl(method = "repeatedcv", repeats = nrif)
risultati <- train(EPS ~ EBITDA+RETURN_ON_ASSET+SUB_INDUSTRY, data= celentano, method = "lm", trControl = controllo)

# Estrazione dei valori della metrica di valutazione (R-squared) su tutte le ripetizioni della cross validation
metrica_di_valutazione <- risultati$resample$Rsquared

# Calcolo della media e della deviazione standard dei valori della metrica di valutazione
media <- mean(metrica_di_valutazione)
deviazione_standard <- sd(metrica_di_valutazione)

# Visualizzazione dei risultati
cat("Media del R2:", round(media, 2), "\n");cat("Deviazione standard R2:", round(deviazione_standard, 2), "\n")
####normalità errori
library(stats)
errori=residuals(reg_lineare)
round(mean(errori))
########################################################
# Fit the linear regression model
reg_lin<- lm(EPS ~ ., data =celentano)

# Specify the values for EBITDA and RETURN_ON_ASSET
new_data <- data.frame(EBITDA = 0.118255, RETURN_ON_ASSET =8.23 ,SUB_INDUSTRY='Electronic and Electrical Equipment')
new_pred <- predict(reg_lin, new_data)
alpha <- 0.05 # significance level
se <- summary(reg_lin)$sigma # residual standard error
qt_val <- qt(1 - alpha/2, df = reg_lin$df.residual) # quantile value
ci_lower <- new_pred - qt_val * se # lower bound of the confidence interval
ci_upper <- new_pred + qt_val * se # upper bound of the confidence interval
print(paste("Confidence Interval (", 1-alpha, "): [", ci_lower, ", ", ci_upper, "]"))
########################################################
######## train test----
celentano <- celentano %>% filter(!(SUB_INDUSTRY=='Tobacco')) 
set.seed(123)
indexes <- createDataPartition(celentano$EPS, p = 0.7, list = FALSE)
train <- celentano[indexes, ]
test <- celentano[-indexes, ]
model <- lm(EPS ~ ., data= train)
summary(model)
predictions <- predict(model, newdata = test)
mse <- mean((test$EPS - predictions)^2)
sqrt(mse)
rmse(test$EPS, predictions)
predict(model, test, interval = "confidence", level = 0.95)
plot(test$`RETURN_ON_ASSET`, test$EPS)
abline(model)
confint(model, level = 0.95)


# Calcolare il valore critico al 95% di livello di confidenza con 3 gradi di libertà residui
t_val <- qt(0.975, df = length(reg_lineare$EBITDA)+2)

# Calcolare l'intervallo di confidenza per un valore di x = 6
R <- predict(reg_lineare, data.frame(SUB_INDUSTRY='Retailers',EBITDA=0.155647, RETURN_ON_ASSET=8.44), interval = "confidence", level = 0.95)
# Stampa l'intervallo di confidenza per R
cat("Intervallo di confidenza è:", R[2], "-", R[3], "\n")

new_pred <- predict(model,data.frame(SUB_INDUSTRY='Retailers',EBITDA=0.155647, RETURN_ON_ASSET=8.44) )
alpha <- 0.05 # significance level
se <- summary(model)$sigma # residual standard error
qt_val <- qt(1 - alpha/2, df = model$df.residual) # quantile value
ci_lower <- new_pred - qt_val * se # lower bound of the confidence interval
ci_upper <- new_pred + qt_val * se # upper bound of the confidence interval
# Print the confidence intervals
print(paste("Confidence Interval (", 1-alpha, "): [", ci_lower, ", ", ci_upper, "]"))
#####log variabili----
pippo<-celentano
pippo$EBITDA<- log(pippo$EBITDA)
pippo$RETURN_ON_ASSET<-log(pippo$RETURN_ON_ASSET)
pippo$EPS<-log(pippo$EPS)
pippo<-drop_na(pippo)
library(caret)
library(Metrics)
set.seed(123)
indexes <- createDataPartition(pippo$EPS, p = 0.7, list = FALSE)
train <- pippo[indexes, ]
test <- pippo[-indexes, ]
model <- lm(EPS ~ ., data= train)
summary(model)
predictions <- predict(model, newdata = test)
mse <- mean((test$EPS - predictions)^2)
mse
rmse(test$EPS, predictions)
plot(pippo$`EBITDA`, pippo$EPS)
abline(model)



