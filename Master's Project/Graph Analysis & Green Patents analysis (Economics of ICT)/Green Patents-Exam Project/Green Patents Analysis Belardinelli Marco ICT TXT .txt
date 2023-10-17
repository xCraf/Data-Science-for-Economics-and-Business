rm(list=ls())
library(readxl)
library(ggplot2)
library(purrr)
library(cluster)
library(factoextra)
library(tidyverse)
library(dplyr)
BM_data <- read_excel("C:/Users/Marco/Downloads/green_patents_terzo_appello.xlsx")
BM_data= unique(BM_data)


######################################## numer of patents every 10 years##############################################
r=cbind(BM_data$Publication_number,BM_data$Publication_year)
r=as.data.frame(r)
r=unique(r)

stocchio=(as.data.frame(table(r$V2)))
stocchio$Var1= as.numeric(stocchio$Var1)+1978
stocchio1=subset(stocchio,Var1< 1990)
stocchio2=subset(stocchio,Var1<2000 & Var1>1989)
stocchio3=subset(stocchio,Var1< 2010 & Var1>1999)
stocchio4=subset(stocchio,Var1< 2020 & Var1>2009)

A= sum(stocchio1$Freq, na.rm=FALSE)
B= sum(stocchio2$Freq, na.rm=FALSE)
C= sum(stocchio3$Freq, na.rm=FALSE)
D= sum(stocchio4$Freq, na.rm=FALSE)
Total= A+B+C+D
Number_of_Patents= rbind(A,B,C,D,Total)

Period=c('1979_1989','1990_1999','2000_2009','2010_2019','Total')
uu=data.frame(Period, Number_of_Patents)

############################## Number of patents over years grafico #####################################
with(stocchio[(stocchio$Var1>=1979) & (stocchio$Var1<=2019),], plot(Var1, Freq, xlab="Year" , ylab="No of patents", main=" Number of patents"))
with(stocchio[(stocchio$Var1>=1979) & (stocchio$Var1<=2019),], lines(Var1, Freq, xlab="Year" , ylab="No of patents", main=" Number of patents"))

############################### Number of patents by applicant country #####################################
d=(cbind(BM_data$Publication_number,BM_data$Currentownerscountrycode))
d=unique(as.data.frame(d))
stacchio=as.data.frame(table(d$V2))
stacchio=stacchio%>%
  arrange(desc(Freq))%>%
  slice(1:10)

ggplot(stacchio, aes(x = reorder(Var1,desc(Freq)), y = Freq)) + 
  geom_bar(position="dodge", stat="identity") +
    labs(
    x = "country",
    y = "number of patents",
    title = "Number of patents by applicants' country(top ten)"
  )+ theme_minimal() 

################################### Number of patents by inventor's country #####################################
s=(cbind(BM_data$Publication_number,BM_data$Inventorscountrycode))
s=unique(as.data.frame(s))
stecchio=as.data.frame(table(s$V2))
stecchio=stecchio%>%
  arrange(desc(Freq))%>%
  slice(1:10)

ggplot(stecchio, aes(x = reorder(Var1,desc (Freq)), y = Freq)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(
    x = "country",
    y = "number of patents",
    title = "Number of patents by inventor's country"
  )+ theme_minimal() 
  

################################### Number of patents for IPC class #####################################
K= unique(BM_data$Publication_number)
K=as.data.frame(substr(BM_data$Publication_number,1,4))
K= as.data.frame(table(K))
colnames(K)=c("IPC","N.")

######################################## Number of patents collaborations #######################################
colla=drop_na(unique(as.data.frame(cbind(BM_data$Publication_number,BM_data$Currentowners))))
collab=as.data.frame(table(colla$V1))
collas= sum(collab$Freq >1)
XX=(collas/Total)*100
Variabile= c("Number of patents with multiple applicants","Number of total patents")
Number= c(collas, Total)
Perc= c(XX, "100")
XXX= cbind(Variabile, Number, Perc)
######################################## Number of international patents collaborations #######################################
collo=drop_na(unique(as.data.frame(cbind(BM_data$Publication_number,BM_data$Currentownerscountrycode))))
colli= as.data.frame(table(collo$V1))
collare= sum(colli$Freq >1)
nocoll= collas-collare
YY=(collare/collas)*100
nointerncoll= 100-YY
Variabile_= c("National Collaborations","International Collaborations","Total Number of Collaborations")
Number_= c(nocoll,collare, collas)
Perc_= c(nointerncoll,YY, "100")
YYY= cbind(Variabile_, Number_, Perc_)

########################### Distribution of Firms by number of patents #########################
data1=(as.data.frame(table(BM_data$Currentowners)))

dataz=data1%>%
  arrange(desc(Freq))%>%
  slice(1:10)

ggplot(dataz, aes(x = reorder(Var1,desc (Freq)), y = Freq)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(
  x = "Firms",
  y = "number of patents",
  title = "Distribution of firms by number of patents"
    )+ theme_minimal() +
  theme(axis.text.x=element_text(angle=45 , hjust=1, size=10))

########################### Distribution of patents by applicant #########################
tot= sum(data1$Freq)
data1$perc= ((data1$Freq) / tot)
data1= data1[order(desc(data1$Freq)),]
colnames(data1)=c("Applicants","Total Number of Patents","perc")
SamC4 <-head(data1, n = 4)
C4_index=sum(SamC4$perc)
print(C4_index)

########################### Distribution of patents by applicant (different periods) #########################
BM1=subset(BM_data,Publication_year<2000)
BM2=subset(BM_data,Publication_year<2010 & Publication_year>1999)
BM3=subset(BM_data,Publication_year>2009)

data2=(as.data.frame(table(BM1$Currentowners)))
data3=(as.data.frame(table(BM2$Currentowners)))
data4=(as.data.frame(table(BM3$Currentowners)))

tot= sum(data2$Freq)
data2$perc= ((data2$Freq) / tot)
data2= data2[order(desc(data2$Freq)),]
data2=data2%>%
  arrange(desc(Freq))%>%
  slice(1:10)
colnames(data2)=c("Applicants (Before 2000)","Total Number of Patents","perc")

tot= sum(data3$Freq)
data3$perc= ((data3$Freq) / tot)
data3= data3[order(desc(data3$Freq)),]
data3=data3%>%
  arrange(desc(Freq))%>%
  slice(1:10)
colnames(data3)=c("Applicants (Between 2001 and 2009))","Total Number of Patents","perc")

tot= sum(data4$Freq)
data4$perc= ((data4$Freq) / tot)
data4= data4[order(desc(data4$Freq)),]
data4=data4%>%
  arrange(desc(Freq))%>%
  slice(1:10)
colnames(data4)=c("Applicants (After 2009)","Total Number of Patents","perc")


years=cbind(data2,data3,data4)


