file.choose()

data2 <- read.csv("/Users/maheshg/Dropbox/Sample Datasets Kaggle/ds_salaries.csv", header = TRUE)

head(data2)
View(data2)

scaled.data2 <- scale(data2[,5:10])

class(data2)


#K means clustering 

file.choose()

KRCDF <- read.csv("/Users/maheshg/Dropbox/Clustering in R Great Learning/Cust_Spend_Data.csv", header = TRUE)

View(KRCDF)
scaled.krcdf <- scale(KRCDF[,3:7])

View(scaled.krcdf)

install.packages("NbClust")
library(NbClust)

set.seed(1234)

nc <- NbClust(KRCDF[,c(-1,-2)], min.nc = 2, max.nc = 4, method = "kmeans")

kmeans.clus <- kmeans(x= scaled.krcdf, center = 2 , nstart = 25)
kmeans.clus

install.packages("fpc")
library(fpc)
plotcluster(scaled.krcdf, kmeans.clus$cluster)

install.packages("cluster")
library(cluster)
clusplot(scaled.krcdf, kmeans.clus$cluster, 
         color = TRUE, shade = TRUE, labels = 2, lines = 1)

#profiling the clusters : 

KRCDF$clusters <- kmeans.clus$cluster
View(KRCDF)

aggr = aggregate(KRCDF[,-c(1,2,8)], list(KRCDF$clusters), mean)
clus.profile <- data.frame(Cluster = aggr[,1],
                           Freq = as.vector(table(KRCDF$clusters)),
                           aggr[,-1])