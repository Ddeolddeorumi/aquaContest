library(cluster)
library(randomForest)

datClusteringHome = read.csv(file.choose())
datModelingHome = read.csv(file.choose())

datKmeansHome = kmeans(datClusteringHome[,2:3], 3, nstart = 20)

datKmeansHome$centers

idHomeCluster1 = datClusteringHome[datKmeansHome$cluster==1, 1]
idHomeCluster2 = datClusteringHome[datKmeansHome$cluster==2, 1]
idHomeCluster3 = datClusteringHome[datKmeansHome$cluster==3, 1]


#sum(datModelingHome$meter_id %in% idHomeCluster1)


datModelingHome1stCluster = datModelingHome[ datModelingHome$meter_id %in% idHomeCluster1,]
datModelingHome2ndCluster = datModelingHome[ datModelingHome$meter_id %in% idHomeCluster2,]
datModelingHome3rdCluster = datModelingHome[ datModelingHome$meter_id %in% idHomeCluster3,]


Home2ndClusterLm = lm(dayUsage~습도 + 기온, data=datModelingHome2ndCluster)
Home2ndClusterLmfit = predict(Home2ndClusterLm, newdata=datModelingHome2ndCluster)

sqrt(mean((datModelingHome2ndCluster$dayUsage - Home2ndClusterLmfit)^2))
