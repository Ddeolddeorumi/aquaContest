#install.packages('cluster')
#library(cluster)

usageDatInfoHome = read.csv(file.choose(), header= T)
weatherDat = read.csv(file.choose(), header = T)

usageDatInfoHome = usageDatInfoHome[,-1]
usageDatInfoHome$dayUsage = apply(usageDatInfoHome[,3:26],1, sum)

#################################
#1. weather를 하루에 관한 것으로#
#################################

bindDate = function(x) #x는 list
{
  zero = c('0','')
  yearMonth = x[1]
  date = x[2]
  date = paste(zero[nchar(date)],date,sep='')
  yearMonthDate = paste(yearMonth,date,sep='')
  return(yearMonthDate)
}

weatherDat$ymd = apply(weatherDat[,c(1,2)],1,bindDate)
weatherDat$ymd = as.factor(weatherDat$ymd)

#기온 습도 풍속 강수량

weatherDatProcessed = aggregate(weatherDat[,4:7],list(weatherDat$ymd),mean)
colnames(weatherDatProcessed)[1] = 'Data' #usage데이터와 merge하기 위함
#plot(weatherDatProcessed$기온)

################################
#2. usageData를 필요한 모양으로#
################################

# length(levels(usageDatInfoHome$meter_id))
# 1462rows

#merge with weather

datModeling = usageDatInfoHome[,c(1,2,29)]
datModeling = merge(datModeling, weatherDatProcessed, by = 'Data', sort=F)
datModeling = datModeling[order(datModeling$meter_id),] # sorting

#write.csv(datModeling, 'datModelingHome.csv', row.names = F)

#finding a correlation

datClustering = NULL
for ( i in 1:length(levels(datModeling$meter_id)) )
{
  ID = datModeling[ datModeling$meter_id == levels(datModeling$meter_id)[i], ]
  Meter_ID = levels(datModeling$meter_id)[i]
  
  if (sum(ID$dayUsage)<=0.002)
  {
    corTemperature = 0
    corHumidity = 0
    corWind = 0
    corPrecipitation = 0
  } else {
    corTemperature = cor(ID$기온,ID$dayUsage)
    corHumidity = cor(ID$습도,ID$dayUsage)
    corWind = cor(ID$풍속,ID$dayUsage)
    corPrecipitation = cor(ID$강수량,ID$dayUsage)
  }
  
  oneLine = data.frame(Meter_ID, corTemperature, corHumidity, corWind, corPrecipitation) 
  datClustering = rbind(datClustering, oneLine)
}

#datModeling[datModeling$meter_id=='17-002593',] 사용량이 너무 적다. obs 하나 뿐이기 때문에 하드코딩.

###################################
###Chosing Cut Point by Plotting###
###################################
a = dist(datClustering[,2:3])

hc.s = hclust(a, method='single')
#cutree(hc.s, 4)
hc.c = hclust(a, method='complete')
#cutree(hc.c, 4)
hc.a = hclust(a, method='average')
#cutree(hc.a, 4)
hc.w = hclust(a, method='ward.D')
#cutree(hc.w, 4)
par(mfrow=c(2,2))
plot(hc.s, main='Single linkage', cex=0.5)
plot(hc.c, main='Complete linkage', cex=0.5)
plot(hc.a, main='Average linkage', cex=0.5)
plot(hc.w, main='Ward linkage', cex=0.5)


##########################
###Summary Using Kmeans###
##########################

giveUsSummaryKmeans = function(datClustering, k = 1)
{
  datKmeans = kmeans(datClustering, k, nstart = 20)
  for (i in 1:k)
  {
    n = sum(datKmeans$cluster==i)
    print(paste('amount of ',i,'th cluster: ',n, sep=''), quote = F)
    print('center', quote = F)
    print(datKmeans$centers[i,])
  }
}
giveUsSummaryKmeans(datClustering[,2:3], k=4) #with no reason, k = 4

##########################
###Summary Using Hclust###
##########################

giveUsSummaryHclust = function(datClustering, k = NULL, method = NULL)
{
  distanceMat = daisy(datClustering)
  hc = hclust(distanceMat, method=method)
  for (i in 1:k)
  {
    n = sum(cutree(hc, k)==i)
    print(paste('amount of ',i,'th cluster: ',n, sep=''), quote = F)
    print('Center Mean', quote = F)
    clusterByHclust = datClustering[cutree(hc, k)==i,]
    print(apply(clusterByHclust,2,mean))
    print('Center Median', quote = F)
    print(apply(clusterByHclust,2,median))
  }
  
}

giveUsSummaryHclust(datClustering[,2:3],k=4, method = 'ward.D')

#write.csv(datClustering, 'datClusteringHome.csv', row.names = F)
?write.csv



