rm(list = ls())
library(readxl)
library(dplyr)
library(ggplot2)

data_priority <- read_excel("C:/Users/Marco/Downloads/R-ICT/Assignment_2/Tech_4g_5g.xlsx")

plot(data_priority$Year, data_priority$TECH_4G, xlab="Year", ylab="Number of patents", main="Number of 4G (blue)-5G (red) patents over years")
plot(data_priority$Year, data_priority$TECH_5G, xlab="Year", ylab="Number of patents", main="Number of 4G (blue)-5G (red) patents over years")
with(data_priority[(data_priority$Year>=1990) & (data_priority$Year<=2021),], 
                                lines(Year, TECH_4G, xlab="Year" , ylab="No of patents",col = "blue", lty = 1, lwd = 2))
with(data_priority[(data_priority$Year>=1990) & (data_priority$Year<=2021),], 
                                lines(Year, TECH_5G, xlab="Year" , ylab="No of patents",col = "red", lty = 1, lwd = 2))
################################################# C4 INDEX ###########################################################
######### 1G ##############
data1g <- read_excel("C:/Users/Marco/Downloads/R-ICT/Assignment_2/Tech_1g.xlsx",sheet = "Applicants")

data1g$total_pat=sum(data1g$Number_of_documents, na.rm = FALSE)

data1g$Share_pat=data1g$Number_of_documents/data1g$total_pat

data1gC4 <-head(data1g, n = 4)
C4_index=sum(data1gC4$Share_pat, na.rm = FALSE)
print(C4_index)
######### 3G ##############
data3g <- read_excel("C:/Users/Marco/Downloads/R-ICT/Assignment_2/Tech_3g.xlsx",sheet = "Applicants")
data3g$total_pat=sum(data3g$Number_of_documents, na.rm = FALSE)

data3g$Share_pat=data3g$Number_of_documents/data3g$total_pat

data3gC4 <-head(data3g, n = 4)
C4_index=sum(data3gC4$Share_pat, na.rm = FALSE)
print(C4_index)
######### 5G #############
data5g <- read_excel("C:/Users/Marco/Downloads/R-ICT/Assignment_2/Tech_5g.xlsx",sheet = "Applicants")
data5g$total_pat=sum(data5g$Number_of_documents, na.rm = FALSE)

data5g$Share_pat=data5g$Number_of_documents/data5g$total_pat

data5gC4 <-head(data5g, n = 4)
C4_index=sum(data5gC4$Share_pat, na.rm = FALSE)
print(C4_index)
################################################# SAMSUNG #######################################################
data_priority <- read_excel("C:/Users/Marco/Downloads/R-ICT/Assignment_2/Samsung.xlsx", sheet = "Earliest priority date")
plot(data_priority$Earliest_priority_date, data_priority$Number_of_documents, xlab="Year", ylab="Number of patents", main="Number of patents from 1990 to 2019")
with(data_priority[(data_priority$Earliest_priority_date>=1990) & (data_priority$Earliest_priority_date<=2021),],
     plot(Earliest_priority_date, Number_of_documents, xlab="Year" , ylab="No of patents", main="Number of patents from 1990 to 2019"))
with(data_priority[(data_priority$Earliest_priority_date>=1990) & (data_priority$Earliest_priority_date<=2021),], 
     lines(Earliest_priority_date, Number_of_documents, xlab="Year" , ylab="No of patents", main="Number of patents from 1990 to 2019"))
################################################# Top Countries #################################################
datapubb <- read_excel("C:/Users/Marco/Downloads/R-ICT/Assignment_2/Samsung.xlsx", sheet = "Inventors - country")

datapubb$tota_pat=sum(datapubb$Number_of_documents, na.rm = FALSE)
datapubb$Share_pat=(datapubb$Number_of_documents/ datapubb$tota_pat)*100

datapubb %>%
  arrange(desc(Number_of_documents)) %>%
  slice(1:10) 

datapubb10 <-head(datapubb , n = 10)

ggplot(datapubb10, aes( y=Share_pat, x= Inventors_country)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Inventors' country") +
  ylab("Share of patents")
################################################# TOP IPC #######################################################
data_ipc<- read_excel("C:/Users/Marco/Downloads/R-ICT/Assignment_2/Samsung.xlsx", sheet="IPC main groups")

data_ipc$ipc4=substr(data_ipc$IPC_main_group,1,4)
 
ipc4_NumPat <- aggregate(x=data_ipc$Number_of_documents, by=list(data_ipc$ipc4), FUN=sum, na.rm=TRUE)

ipc4_NumPat <- ipc4_NumPat %>% rename( IPC4 = Group.1 )
ipc4_NumPat <- ipc4_NumPat %>% rename( Num_pat = x )

ipc4_NumPat <- ipc4_NumPat %>% arrange(desc(Num_pat))

write.table(ipc4_NumPat, file ="C:/Users/Marco/Downloads/R-ICT/Assignment_2/Samsung_IPC.xlsx", sep = ",", quote = FALSE, row.names = F)
########## C4 OF TOP IPC ############
C4Sam <- ipc4_NumPat
  
C4Sam$total_pat=sum(C4Sam$Num_pat, na.rm = FALSE)

C4Sam$Share_pat=C4Sam$Num_pat/C4Sam$total_pat

SamC4 <-head(C4Sam, n = 4)
C4_index=sum(SamC4$Share_pat, na.rm = FALSE)
print(C4_index)
################################################# AFTER & BEFORE #################################################
data_ipc1<- read_excel("C:/Users/Marco/Downloads/R-ICT/Assignment_2/SAMSUNG_BEFORE_2000.xlsx", sheet="IPC main groups")

data_ipc1$ipc4=substr(data_ipc1$IPC_main_groups,1,4)
 
ipc4_NumPat1 <- aggregate(x=data_ipc1$Number_of_documents, by=list(data_ipc1$ipc4), FUN=sum, na.rm=TRUE)

ipc4_NumPat1 <- ipc4_NumPat1 %>% rename( IPC4 = Group.1 )
ipc4_NumPat1 <- ipc4_NumPat1 %>% rename( Num_pat = x )

ipc4_NumPat1 <- ipc4_NumPat1 %>% arrange(desc(Num_pat))
write.table(ipc4_NumPat1, file ="C:/Users/Marco/Downloads/R-ICT/Assignment_2/Samsung_IPC_Before.xlsx", sep = ",", quote = FALSE, row.names = F)
######## After #########
data_ipc2<- read_excel("C:/Users/Marco/Downloads/R-ICT/Assignment_2/SAMSUNG_AFTER_2000.xlsx", sheet="IPC main groups")

data_ipc2$ipc4=substr(data_ipc2$IPC_main_groups,1,4)
 
ipc4_NumPat2 <- aggregate(x=data_ipc2$Number_of_documents, by=list(data_ipc2$ipc4), FUN=sum, na.rm=TRUE)

ipc4_NumPat2 <- ipc4_NumPat2 %>% rename( IPC4 = Group.1 )
ipc4_NumPat2 <- ipc4_NumPat2 %>% rename( Num_pat = x )

ipc4_NumPat2 <- ipc4_NumPat2 %>% arrange(desc(Num_pat))
write.table(ipc4_NumPat, file ="C:/Users/Marco/Downloads/R-ICT/Assignment_2/Samsung_IPC_After.xlsx", sep = ",", quote = FALSE, row.names = F)
################################################# H01L ###########################################################
HOIL<- read_excel("C:/Users/Marco/Downloads/R-ICT/Assignment_2/H01L.xlsx",sheet = "Applicants")

HOIL$ipc4=substr(HOIL$Applicants,1,4)

OIL <- aggregate(x=HOIL$`Number of documents`, by=list(HOIL$ipc4), FUN=sum, na.rm=TRUE)

OIL <- OIL %>% rename( Applicants = Group.1 )
OIL <- OIL %>% rename( Num_pat = x )

OIL <- OIL %>% arrange(desc(Num_pat))

OIL <-head(OIL, n = 10)
write.table(ipc4_NumPat, file ="C:/Users/Marco/Downloads/R-ICT/Assignment_2/H01L_TOP10.xlsx", sep = ",", quote = FALSE, row.names = F)



