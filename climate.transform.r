#평균 기후 만들기
climate<-read.csv('climate.csv')
str(climate)

climate$hour = substr(climate$hour,1,nchar(climate$hour)-2)
for(i in 1:nrow(climate)) {
  if(climate[i,3]=="") {climate[i,3]<-0}
}
str(climate)
climate <- climate %>% rename(humidity=습도,rain=강수량, temp=기온,wind=풍속)
str(climate)
climate$hour<-NULL

n.colmeans = function(df, n){
  aggregate(x = df,
            by = list(gl(ceiling(nrow(df)/n), n)[1:nrow(df)]),
            FUN = mean)
}

cli.mean<-n.colmeans(climate, 24)

#일별 날짜로 변환
library(tidyr)
cli.mean <- cli.mean %>% unite(date,X, format..day, sep = "", remove = FALSE) 
cli.mean[,c(1,3,4)]<-NULL
str(cli.mean)

for (i in 1:nrow(cli.mean)) {
  if(nchar(cli.mean[i,1])<8) 
    {cli.mean[i,1]<-paste(substr(cli.mean[i,1],1,6),substr(cli.mean[i,1],7,7),sep='0')}
}
cli.mean$date<-anydate(cli.mean$date)

#최고, 최저 기온 
climate <- climate %>% unite(date,X, format..day, sep = "", remove = FALSE) 
climate[,c(2,3)]<-NULL

for (i in 1:nrow(climate)) {
  if(nchar(climate[i,1])<8) 
  {climate[i,1]<-paste(substr(climate[i,1],1,6),substr(climate[i,1],7,7),sep='0')}
}
climate$date<- anydate(climate$date)
high.temp <- climate %>% group_by(date) %>% summarize(high.temp=max(temp)) 
low.temp <- climate %>% group_by(date) %>% summarize(low.temp=min(temp)) 
tempHL<- full_join(high.temp, low.temp, by='date')

#기상데이터-평균(습도, 온도, 풍속, 강수량), 최고기온, 최저기온
cli.new <- full_join(tempHL, cli.mean, by='date')
write.csv(cli.new, 'cli.new.csv', row.names=F)
