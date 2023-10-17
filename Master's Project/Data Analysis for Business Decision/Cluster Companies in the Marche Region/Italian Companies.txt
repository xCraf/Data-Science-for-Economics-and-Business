rm(list = ls())
library(readxl)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(corrplot)
library(dplyr)
library(rstudioapi)
## importiamo---- 
current_path <- getActiveDocumentContext()$path
mobile_train_df <- read_excel("data.xlsx")
dim(mobile_train_df)
str(mobile_train_df)
head(mobile_train_df)
summary(mobile_train_df)
mobile_train_df=drop_na(mobile_train_df) 
duplicated(mobile_train_df)
mobile_train_df$`Product Type` <- as.numeric(as.factor(mobile_train_df$`Product Type`))
#cambio segno agli sconti 
mobile_train_df$Discounts <- -mobile_train_df$Discounts 
#elimino osservazioni che hanno sconto maggiore di gross sales 
mobile_train_df <- mobile_train_df %>% filter(!(`Gross Sales` < Discounts)) 
#elimino osservazioni che non rispettano `Gross Sales` - Discounts - `Net Sales` = 0 
mobile_train_df <- mobile_train_df %>%   filter((`Gross Sales` - Discounts - `Net Sales` == 0))
mobile_train_df=mobile_train_df[,-16]
mobile_train_df$"Total Sales"=mobile_train_df$"Net Sales"+mobile_train_df$Taxes
mobile_train_df$Discounts <- -mobile_train_df$Discounts
# net quantity = order quantity?
mobile_train_df <- mobile_train_df %>% filter((`Net Quantity` != 'Ordered Item Quantity'))


## correlazioni----
numeric.var <- sapply(mobile_train_df, is.numeric)
str(numeric.var)
C <- cor(mobile_train_df[,numeric.var])
dev.new()
corrplot(C, method = 'number')
corrplot(C, method = 'circle')


## rimuovo feat inutili e normalizzo----
data <- mobile_train_df[, !colnames(mobile_train_df) %in% c("Record ID","Order ID","Product Title","Variant Title","Variant Sku","Variant Sku","Variant ID","Day","Ordered Item Quantity","Gross Sales","Discounts","Total Sales","Taxes","Returns")]

data$'Net Sales'<- scale(mobile_train_df$`Net Sales`)
#data$'Discounts'<- scale(mobile_train_df$`Discounts`)
data$'Net Quantity'<- scale(mobile_train_df$`Net Quantity`)


numeric.var <- sapply(data, is.numeric)
str(numeric.var)
C <- cor(data[,numeric.var])
dev.new()
corrplot(C, method = 'number')
corrplot(C, method = 'circle')

data$`Client ID`<- as.numeric(data$`Client ID`)

###dataset nuovo----
new<- data[,c("Client ID","Product Type","Net Quantity","Net Sales","Returned Item Quantity")]


library(dplyr)
            
df1=new %>% 
  group_by(new$`Client ID`) %>% 
  summarise(NetQuantity = sum(`Net Quantity`),
            ReturnedQuantity=sum(`Returned Item Quantity`),
            NetSales = sum(`Net Sales`))


library(factoextra)
## Elbow----
fviz_nbclust(df1[,-c(1)], kmeans, method = "wss")

library(cluster)
# simil-elbow----
#calculate gap statistic based on number of clusters
gap_stat <- clusGap(new[,-c(1)],
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)


################# test silhouette------
seeds <- c(123, 234, 678, 1234, 2345) 
results <- list() 
for (i in 1:length(seeds)) { 
  set.seed(seeds[i]) 
  kmeans_result <- kmeans(df1[,-1], centers = 3) 
  silhouette_vec <- silhouette(kmeans_result$cluster, dist(scale(df1))) 
  silhouette_avg <- mean(silhouette_vec[, 3]) 
  results[[i]] <- list(seed = seeds[i], silhouette = silhouette_avg) } 
results_df <- as.data.frame(do.call(rbind, results)) 
print(results_df)

################K-means-----
set.seed(123)
kmeans_result <- kmeans(df1[,-c(1)], centers = 3)

kmeans_result
kmeans_result$cluster
kmeans_result$centers
table(kmeans_result$cluster)

############### merge dataset aggregati e non con plot------
dati_belli <- data.frame(df1, cluster = as.factor(kmeans_result$cluster))
dati_belli<- dati_belli[,-c(2,3,4)]
names(dati_belli)[which(names(dati_belli) == "new..Client.ID.")] <- "Client ID"
Pupo <- merge(mobile_train_df, dati_belli, by = "Client ID")
library(rgl)
plot3d(Pupo$`Product Type`,Pupo$`Net Sales`,Pupo$`Net Quantity`,
       col = Pupo$cluster, size = 10)


Albano=Pupo %>% 
  group_by(`Client ID`) %>% 
  summarise(NetQuantity = sum(`Net Quantity`),
            ReturnedQuantity=sum(`Returned Item Quantity`),
            GrossSales = sum(`Gross Sales`),
            Discounts = sum(`Discounts`),
            Taxes = sum(`Taxes`),
            OrderedItemQuantity = sum(`Ordered Item Quantity`),
            TotalSales = sum(`Total Sales`),
            NetSales = sum(`Net Sales`))

Romina <- merge(Albano, dati_belli, by = "Client ID")

plot3d(Romina$NetQuantity,Romina$NetSales,Romina$Discounts,
       col = Romina$cluster, size = 10)
#library(openxlsx)
#write.xlsx(Romina, file = "C:/Users/Marco/Downloads/Romina.xlsx", sheetName = "Nome_foglio", rowNames = FALSE)
#################fviz cluster------

library(ggpubr)
fviz_cluster(kmeans_result, data = df1,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800","blue","red","yellow"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)




