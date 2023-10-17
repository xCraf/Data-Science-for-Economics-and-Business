rm(list=ls())
library(readxl)
library(ggplot2)
library(purrr)
library(cluster)
library(factoextra)
BM_data_assignment <- read_excel("C:/Users/Marco/Downloads/R-ICT/BM_data_assignment.xlsx")
scaled_data= BM_data_assignment[,2:13]
scaled_data=scale(subset(BM_data_assignment[,2:13]))

# le righe 8 e 9 sono invertibili, la riga 8 utilizza dati non scalati mentre nella 9 sono scalati
# tuttavia, avendo come risultato un cluster "migliore" utilizzando i dati scalati 
# ho pensato fosse opportuno utilizzarli come principali, sul Power Point sono presenti i dati 
# risultanti dal codice che sfrutta i dati scalati.

# Tecnica della silhouette per capire quanti cluster convenga creare
# l'esecuzione di questa funzione richiede parecchio tempo, il risultato comunque è 4
sil_width <- map_dbl(2:10, function(k){
  model <- pam(x = scaled_data, k = k)
  model$silinfo$avg.width
})
sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)
ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() + geom_point() +011111111111
  scale_x_continuous(breaks = 2:10)

# k-means per la creazione dei cluster
km=kmeans(scaled_data, centers=4, nstart =110)
fviz_cluster(km, data=scaled_data, ggtheme=theme_classic())

# ordinamento dei dati
BM_data_assignment=cbind(BM_data_assignment, km$cluster)
BM=table(BM_data_assignment[,14])
BM=as.data.frame(BM)
BM[,3]=(BM[,2]/nrow(BM_data_assignment))*100
colnames(BM)=c('BM', 'Number of firms', '% of firms')
rownames(BM)=c('BM1','BM2','BM3','BM4')
View(BM)
View(km$center)

