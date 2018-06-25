usageDat = read.csv(file.choose(), header = T)
weatherDat = read.csv(file.choose(), header = T) #근데 왜 4자리까지 밖에 안 되지?

usageDat$dayUsage = apply(usageDat[,3:26],1, sum)

#1. weather를 하루에 관한 것으로


#yearMonth = weatherDat[245,1]
#date = weatherDat[245,2]
#date = paste(zero[nchar(date)],date,sep='')
#paste(yearMonth,date,sep='')

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
plot(weatherDatProcessed$기온)


#2. usageData를 필요한 모양으로

##ID개수 확인
#length(levels(usageDat$Meter_ID))
#1608

datModeling = usageDat[,c(1,2,27)]
datModeling = merge(datModeling, weatherDatProcessed, by = 'Data', sort=F)
datModeling = datModeling[order(datModeling$Meter_ID),]

##ID별 유형 탐색
##cor구하기기

#plot(datModeling$dayUsage[1:2000],datModeling$기온[1:2000],xlim = c(0,2))

datClustering = NULL
for ( i in 1:length(levels(datModeling$Meter_ID)) )
{
  ID = datModeling[ datModeling$Meter_ID == levels(datModeling$Meter_ID)[i], ]
  Meter_ID = levels(datModeling$Meter_ID)[i]
  
  if (sum(ID$dayUsage)<=0.00005)
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

############
###scaled###
############

datModeling[,4:7] = scale(datModeling[,4:7])
datModeling[,3] = datModeling[,3]/sd(datModeling[,3])

datClustering = NULL
for ( i in 1:length(levels(datModeling$Meter_ID)) )
{
  ID = datModeling[ datModeling$Meter_ID == levels(datModeling$Meter_ID)[i], ]
  Meter_ID = levels(datModeling$Meter_ID)[i]

  if (sum(ID$dayUsage)<=0.00005)
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

#datClustering[!complete.cases(datClustering),] #Na 검사
#ID = datModeling[ datModeling$Meter_ID == '17-002593', ] #이상한 데이터 이틀밖에 측정이 안 돼  있다.
?kmeans

datKmeans = kmeans(datClustering[,2:5], 4, nstart = 20)

plot(datModeling[,4:6], col=(km$cluster+1), pch=20, cex=2)

datClustering[datKmeans$cluster==1,]

plot(sort(datClustering$corHumidity))
plot(sort(datClustering$corTemperature))
plot(sort(datClustering$corWind))
plot(sort(datClustering$corHumidity))

