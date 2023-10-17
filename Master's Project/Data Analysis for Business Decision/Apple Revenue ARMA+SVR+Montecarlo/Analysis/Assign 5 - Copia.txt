rm(list=ls())
library(rstudioapi)
library(openxlsx)
library(e1071)
library(readxl)
library(caretForecast)
library(forecast)
library(dplyr)
library(openxlsx)
current_path <- getActiveDocumentContext()$path
Pippo<- read_xlsx('datalag.xlsx')
ts_data <- ts(Pippo[, c("semidiff","gdpdiff","businessdiff","cpidiff","brentdiff","ricavidiff")], start = c(2007, 4), end = c(2022, 4), frequency = 4)

training_data <- window(ts_data, start = c(2007, 4), end=c(2021,4))
#testing_data <- window(ts_data, start = c(2021,3))
testing_datalag<-window(ts_data, start = c(2021,4),end=c(2022,1))

svr.model <- svm(ricavidiff ~ ., data = training_data, kernel = "polynomial",degree=3,coef0=1, cost = 1)


pred <- predict(svr.model, data.frame(testing_datalag), type="response")
pred
resid <- resid(svr.model)
se <- sd(resid)
alpha <- 0.05
tcrit <- qt(1-alpha/2, df=length(resid)-1)
ci_lower <- pred - tcrit * se
ci_upper <- pred + tcrit * se
results <- data.frame(testing_datalag, pred, ci_lower, ci_upper)
print(results)

# Montecarlo Simulation with value taken from ARIMA Model----
# montecarlo 2 trimestre scenario neutro 
gdpdiff=rnorm(n = 10000000, mean = 472.812, sd = 276.84)
businessdiff=rnorm(n = 10000000, mean = -0.23634, sd = 0.74476)
semidiff=rnorm(n = 10000000, mean = 0.181957, sd = 0.410336)
cpidiff=rnorm(n = 10000000, mean = 0.207089, sd = 0.569877)
brentdiff=rnorm(n = 10000000, mean = 5.32025, sd = 6.945852)
testing= data.frame(gdpdiff,businessdiff,semidiff,brentdiff,cpidiff)
pred <- predict(svr.model, data.frame(testing), type="response")
pred
resid <- resid(svr.model)
se <- sd(resid)
alpha <- 0.05
tcrit <- qt(1-alpha/2, df=length(resid)-1)
ci_lower <- pred - tcrit * se
ci_upper <- pred + tcrit * se
results <- data.frame(testing, pred, ci_lower, ci_upper)
x=mean(results$ci_lower)
y=mean(pred)
z=mean(results$ci_upper)
x;y;z

# montecarlo 3 trimestre scenario neutro da sostituire i numeri
gdpdiff=rnorm(n = 10000000, mean = 395.035, sd = 335.4386)
businessdiff=rnorm(n = 10000000, mean = -0.25984, sd = 0.77972)
semidiff=rnorm(n = 10000000, mean = 0.1, sd = 0.33402)
cpidiff=rnorm(n = 10000000, mean = 0.513828, sd = 0.628394)
brentdiff=rnorm(n = 10000000, mean = -2.2353, sd = 7.496739)
testing= data.frame(gdpdiff,businessdiff,semidiff,brentdiff,cpidiff)
pred <- predict(svr.model, data.frame(testing), type="response")
pred
resid <- resid(svr.model)
se <- sd(resid)
alpha <- 0.05
tcrit <- qt(1-alpha/2, df=length(resid)-1)
ci_lower <- pred - tcrit * se
ci_upper <- pred + tcrit * se
results <- data.frame(testing, pred, ci_lower, ci_upper)
x=mean(results$ci_lower)
y=mean(pred)
z=mean(results$ci_upper)
x;y;z

# montecarlo 2 trimestre scenario negativo semiconduttori aumentano di prezzo o gdp negativo 
gdpdiff=rnorm(n = 10000000, mean = 472.812, sd = 276.84)
businessdiff=rnorm(n = 10000000, mean = -0.23634, sd = 0.74476)
semidiff=rnorm(n = 10000000, mean = 1.181957, sd = 0.410336)##
cpidiff=rnorm(n = 10000000, mean = 0.207089, sd = 0.569877)
brentdiff=rnorm(n = 10000000, mean = 5.32025, sd = 6.945852)
testing= data.frame(gdpdiff,businessdiff,semidiff,brentdiff,cpidiff)
pred <- predict(svr.model, data.frame(testing), type="response")
pred
resid <- resid(svr.model)
se <- sd(resid)
alpha <- 0.05
tcrit <- qt(1-alpha/2, df=length(resid)-1)
ci_lower <- pred - tcrit * se
ci_upper <- pred + tcrit * se
results <- data.frame(testing, pred, ci_lower, ci_upper)
x=mean(results$ci_lower)
y=mean(pred)
z=mean(results$ci_upper)
x;y;z
# montecarlo 3 trimestre scenario negativo 
gdpdiff=rnorm(n = 10000000, mean = 395.035, sd = 335.4386)
businessdiff=rnorm(n = 10000000, mean = -0.25984, sd = 0.77972)
semidiff=rnorm(n = 10000000, mean = 1.1, sd = 0.33402)##
cpidiff=rnorm(n = 10000000, mean = 0.513828, sd = 0.628394)
brentdiff=rnorm(n = 10000000, mean = -2.2353, sd = 7.496739)
testing= data.frame(gdpdiff,businessdiff,semidiff,brentdiff,cpidiff)
pred <- predict(svr.model, data.frame(testing), type="response")
pred
resid <- resid(svr.model)
se <- sd(resid)
alpha <- 0.05
tcrit <- qt(1-alpha/2, df=length(resid)-1)
ci_lower <- pred - tcrit * se
ci_upper <- pred + tcrit * se
results <- data.frame(testing, pred, ci_lower, ci_upper)
x=mean(results$ci_lower)
y=mean(pred)
z=mean(results$ci_upper)
x;y;z

# montecarlo 2 trimestre scenario positivo semiconduttori scendono di prezzo o gdp/ business/cpi sale
gdpdiff=rnorm(n = 10000000, mean = 650, sd = 276.84)##
businessdiff=rnorm(n = 10000000, mean = -0.23634, sd = 0.74476)
semidiff=rnorm(n = 10000000, mean = 0.181957, sd = 0.410336)
cpidiff=rnorm(n = 10000000, mean = 0.513828, sd = 0.569877)
brentdiff=rnorm(n = 10000000, mean = 5.32025, sd = 6.945852)
testing= data.frame(gdpdiff,businessdiff,semidiff,brentdiff,cpidiff)
pred <- predict(svr.model, data.frame(testing), type="response")
pred
resid <- resid(svr.model)
se <- sd(resid)
alpha <- 0.05
tcrit <- qt(1-alpha/2, df=length(resid)-1)
ci_lower <- pred - tcrit * se
ci_upper <- pred + tcrit * se
results <- data.frame(testing, pred, ci_lower, ci_upper)
x=mean(results$ci_lower)
y=mean(pred)
z=mean(results$ci_upper)
x;y;z
# montecarlo 3 trimestre scenario positivo
gdpdiff=rnorm(n = 10000000, mean = 600, sd = 335.4386)##
businessdiff=rnorm(n = 10000000, mean = -0.25984, sd = 0.77972)
semidiff=rnorm(n = 10000000, mean = 0.1, sd = 0.33402)
cpidiff=rnorm(n = 10000000, mean = 0.513828, sd = 0.628394)
brentdiff=rnorm(n = 10000000, mean = -2.2353, sd = 7.496739)
testing= data.frame(gdpdiff,businessdiff,semidiff,brentdiff,cpidiff)
pred <- predict(svr.model, data.frame(testing), type="response")
pred
resid <- resid(svr.model)
se <- sd(resid)
alpha <- 0.05
tcrit <- qt(1-alpha/2, df=length(resid)-1)
ci_lower <- pred - tcrit * se
ci_upper <- pred + tcrit * se
results <- data.frame(testing, pred, ci_lower, ci_upper)
x=mean(results$ci_lower)
y=mean(pred)
z=mean(results$ci_upper)
x;y;z










#### terzo
gdpdiff=rnorm(n = 10000000, mean = 367.442, sd = 399.5431)
businessdiff=rnorm(n = 10000000, mean = -0.26831, sd = 0.53929)
semidiff=rnorm(n = 10000000, mean = -0.13163, sd = 0.450573)
cpidiff=rnorm(n = 10000000, mean = -0.326279, sd = 0.745713)
brentdiff=rnorm(n = 10000000, mean = -16.396855, sd = 7.515141)
testing= data.frame(gdpdiff,businessdiff,semidiff,brentdiff,cpidiff)
pred <- predict(svr.model, data.frame(testing), type="response")
pred
resid <- resid(svr.model)
se <- sd(resid)
alpha <- 0.05
tcrit <- qt(1-alpha/2, df=length(resid)-1)
ci_lower <- pred - tcrit * se
ci_upper <- pred + tcrit * se
results <- data.frame(testing, pred, ci_lower, ci_upper)
x=mean(results$ci_lower)
y=mean(pred)
z=mean(results$ci_upper)
x;y;z
## terzo negativo
gdpdiff=rnorm(n = 10000000, mean = 367.442, sd = 399.5431)
businessdiff=rnorm(n = 10000000, mean = -0.26831, sd = 0.53929)
semidiff=rnorm(n = 10000000, mean = 1.1, sd = 0.450573)
cpidiff=rnorm(n = 10000000, mean = -0.326279, sd = 0.745713)
brentdiff=rnorm(n = 10000000, mean = -16.396855, sd = 7.515141)
testing= data.frame(gdpdiff,businessdiff,semidiff,brentdiff,cpidiff)
pred <- predict(svr.model, data.frame(testing), type="response")
pred
resid <- resid(svr.model)
se <- sd(resid)
alpha <- 0.05
tcrit <- qt(1-alpha/2, df=length(resid)-1)
ci_lower <- pred - tcrit * se
ci_upper <- pred + tcrit * se
results <- data.frame(testing, pred, ci_lower, ci_upper)
x=mean(results$ci_lower)
y=mean(pred)
z=mean(results$ci_upper)
x;y;z
## terzo positivo
gdpdiff=rnorm(n = 10000000, mean = 600.442, sd = 399.5431)
businessdiff=rnorm(n = 10000000, mean = -0.26831, sd = 0.53929)
semidiff=rnorm(n = 10000000, mean = -0.13163, sd = 0.450573)
cpidiff=rnorm(n = 10000000, mean = -0.326279, sd = 0.745713)
brentdiff=rnorm(n = 10000000, mean = -16.396855, sd = 7.515141)
testing= data.frame(gdpdiff,businessdiff,semidiff,brentdiff,cpidiff)
pred <- predict(svr.model, data.frame(testing), type="response")
pred
resid <- resid(svr.model)
se <- sd(resid)
alpha <- 0.05
tcrit <- qt(1-alpha/2, df=length(resid)-1)
ci_lower <- pred - tcrit * se
ci_upper <- pred + tcrit * se
results <- data.frame(testing, pred, ci_lower, ci_upper)
x=mean(results$ci_lower)
y=mean(pred)
z=mean(results$ci_upper)
x;y;z
