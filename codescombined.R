#getwd()
#setwd("/Users/Dana/Desktop/ONGOING/water/data")
#read.csv("cus_new.csv", fileEncoding = "euc-kr")

##########################################
########## Water Combined Codes ##########
##########################################


########### 1) 가장 기본적인 전처리 by python ###########
## water usage파일의 원 데이터를 Y축을 ID, X축을 시간으로 변경하여 펼침
## 아무 의미 없이 행 전체가 NA인 행들이 존재하여 해당 행들 일괄 삭제
## 문의해보니 cumulative값을 기준으로 하라 하여 모든 usage 값을 cumulative 값을 통해 재계산
## 결과: "water_usage_dataset_new_180702.csv" 파일
#########################################################


####### 2) 기존의 customer데이터에서 필요 없는 열 삭제 #######
## No., 수용가번호, 주소, 소블럭, 급수상태, 구경 variables 삭제
##############################################################

custdata <- read.csv("customer_info.csv",header=T)
custdata[,c(1,2,4,5,7,8)] <- NULL
View(custdata) 


########## 3) water usage와 customer 데이터를 병합 ##########
## 결과: "customer_and_waterusage_merged_new_180702.csv" 파일
#############################################################

library(dplyr)

originaldata <- read.csv(file.choose(),header=T) #water_usage_dataset_new_180702.csv
customerdata <- read.csv(file.choose(),header=T) #customer_info.csv

colnames(originaldata)[1] = "meter_id"

merged <- full_join(originaldata,customerdata,by=c("meter_id"='meter_id'))

colnames(merged)[31] = 'type'
merged$type = as.character(merged$type)
merged$type[merged$type == '가정용'] = 'home'
merged$type[merged$type == '일반용'] = 'general'
merged$type[merged$type == '학교용'] = 'school'

merged$type = factor(merged$type)
merged = merged[,c(1:26,31)]
#levels(merged$type)

write.csv(merged,file="customer_and_waterusage_merged_new_180702.csv", row.names = F)


###### 4) water usage의 outlier(negative 포함) 값들을 median으로 치환 ######
## 이 때 outlier는 0~1 밖의 범위로 지정, median은 해당 시간의 모든 id의 usage값 중에 median
## 결과: "usageDatInfoHome_new" 파일 (General,School도 있음)
############################################################################

usageDatInfo = read.csv(file.choose(), header = T) #customer_and_waterusage_merged_new_180702.csv

#levels(usageDatInfo$type)

usageDatInfoGeneral = usageDatInfo[usageDatInfo$type=='general',]
usageDatInfoHome = usageDatInfo[usageDatInfo$type=='home',]
usageDatInfoSchool = usageDatInfo[usageDatInfo$type=='school',]

putMedianIn = function(usageDatInfo, criterionHigher, criterionLower = 0)
{
  x = usageDatInfo
  x[,-c(3:26)] = 0
  
  matX = matrix(0, dim(x)[1],dim(x)[2])
  for ( i in 1:dim(x)[1])
  {
    for ( j in 1:dim(x)[2])
    {
      matX[i,j] = paste(i,j,sep=',')
    }
  }
  
  coordinateOfTheOutOfCriterion = matX[x>criterionHigher|x<criterionLower]
  
  for ( i in coordinateOfTheOutOfCriterion )
  {
    xyAxis = strsplit(i,split=',')
    xAxis = as.numeric(xyAxis[[1]][1])
    yAxis = as.numeric(xyAxis[[1]][2])
    
    date = usageDatInfo[xAxis,2]
    sameDateHour = usageDatInfo[usageDatInfo[,2]==date,yAxis]
    sameDateHour = sameDateHour[sameDateHour<=criterionHigher&sameDateHour>=criterionLower]
    if (sum(sameDateHour<=criterionHigher&sameDateHour>=criterionLower)==0)
    {
      sameDateHour = 0
    }
    usageDatInfo[xAxis,yAxis] = median(sameDateHour)
  }
  return(usageDatInfo)
}

usageDatInfoGeneral_new = putMedianIn(usageDatInfoGeneral,1)
usageDatInfoSchool_new = putMedianIn(usageDatInfoSchool,1)
usageDatInfoHome_new = putMedianIn(usageDatInfoHome,1)

write.csv(usageDatInfoGeneral_new, file = "usageDatInfoGeneral_new.csv")
write.csv(usageDatInfoSchool_new, file = "usageDatInfoSchool_new.csv")
write.csv(usageDatInfoHome_new, file = "usageDatInfoHome_new.csv")


#### 5) 시간대별 기상데이터를 일별 기상데이터로 정리 ####
## 파생변수 생성 등 여러 전처리 (평균, 최저, 최고 등)
## 결과: “cli.new.csv” 파일임
#########################################################

# 평균 기후 만들기
climate<-read.csv('climate.csv')
str(climate)

climate$hour = substr(climate$hour,1,nchar(climate$hour)-2)
for(i in 1:nrow(climate)) {
  if(climate[i,3]=="") {climate[i,3]<-0}
}
str(climate)
climate <- climate %>% rename(humidity=습도,rain=강수량, temp=기온, wind=풍속)
str(climate)
climate$hour<-NULL

n.colmeans = function(df, n){
  aggregate(x = df,
            by = list(gl(ceiling(nrow(df)/n), n)[1:nrow(df)]),
            FUN = mean)
}

cli.mean<-n.colmeans(climate, 24)

# 일별 날짜로 변환
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

# 일별 기상데이터 - 평균(습도,온도,풍속,강수량), 최고기온, 최저기온
cli.new <- full_join(tempHL, cli.mean, by='date')
write.csv(cli.new, 'cli.new.csv', row.names=F)


############### 6) "usageDatInfoHome_new"데이터로 계속 ###############
## 시간별 데이터에서 일별 데이터로 변환
## 이 때 일자마다 미터기(id)의 갯수가 다르므로 sum 대신에 mean 값을 사용함
## 날짜 파생변수 추가 — 주말.공휴일 1 평일 0 // 설 추석 (명절) 1 제외 0
## 일별로 정리되어 있는 기상데이터 “cli.new.csv”  파일을 불러와 join함
## 결과: "(mean)water_holi_cli_home.csv” 파일
######################################################################

library(dplyr)
home<-read.csv('usageDatInfoHome_new.csv')
View(home)
home$X<-NULL
home.day<-home %>% mutate(day=rowSums(select(home,contains('X')))) # adding a column with sums of a day
home.day<-home.day %>% select(-(X0:X23))
home.day.mean<-home.day %>% group_by(Data) %>% summarize(date.mean=mean(day)) # calculate daily mean
str(home.day.mean) # !!!we were still missing 2 days here



library(anytime)
home.day.mean$Data <- anydate(home.day.mean$Data)
home.day.mean$weekday <- weekdays(home.day.mean$Data)
home.day.mean$holiday <- ifelse(home.day.mean$weekday=="Saturday" | home.day.mean$weekday=="Sunday", 1, 0)
home.day.mean$holiday <- ifelse(as.character(home.day.mean$Data) %in% c("2016-10-03","2017-03-01","2017-05-03",
                                                                        "2017-05-05","2017-05-09","2017-06-06",
                                                                        "2017-08-15","2017-10-02","2017-10-03",
                                                                        "2017-10-09","2017-12-25"),
                                1, home.day.mean$holiday)

home.day.mean$new_thanks <- ifelse(as.character(home.day.mean$Data) %in% c("2017-01-27","2017-01-28","2017-01-29",
                                                                           "2017-01-30","2017-10-01","2017-10-02",
                                                                           "2017-10-03","2017-10-04","2017-10-05",
                                                                           "2017-10-06","2017-10-07","2017-10-08",
                                                                           "2017-10-09"),1,0)


cli.new<-read.csv('cli.new.csv')
cli.new$date <- anydate(cli.new$date)
home.day.newmean <- full_join(home.day.mean,cli.new,by=c('Data'='date')) 
home.day.newmean$rain <- ifelse(home.day.newmean$rain==0,0,1)
home.day.newmean <- home.day.newmean %>% rename('date'='Data')
home.day.newmean<-home.day.newmean[order(home.day.newmean$date),]
rownames(home.day.newmean)<-1:457

View(home.day.newmean)
#removing needless column 
home.day.newmean$weekday <- NULL

#fill holi, new_thanks values for 20171211 20171212
home.day.newmean[437:438,3:4]<-c(0,0)


#home.day.newmean$high.temp <- NULL
#home.day.newmean$low.temp <- NULL
View(home.day.newmean)

write.csv(home.day.newmean, "(mean)water_holi_cli_home.csv", row.names=F)


########### 7) 존재치 않거나 오류가 있는 행 MICE IMPUTATION ###########
## 2017년 12월에 아예 존재치 않는 row들을 발견하여 usage값을 impute
## 2017년 12월 4일, 7~14일 총 9개의 obs. 에 부재/오류가 있다고 판단
#######################################################################
install.packages("VIM")
install.packages("robustbase")
require(VIM)
install.packages("mice")
require(mice) 

holicli <- read.csv("(mean)water_holi_cli_home.csv",header=T)
holicli_new = holicli$date = NULL
holicli_new = holicli$high.temp = NULL
holicli_new = holicli$low.temp = NULL

holicli_new$holiday <- factor(holicli_new$holiday)
holicli_new$new_thanks <- factor(holicli_new$new_thanks)
holicli_new$rain <- factor(holicli_new$rain)

# NA 있는지 확인
View(holicli)
aggr(holicli,prop=FALSE,numbers=TRUE)

# 인위적으로 NA 만들어보기
holicli[430,1] = NA
holicli[433:440,1] = NA

# MICE
imp <- mice(holicli,m=5,seed=828)     # dataset을 m개 만든다. 디폴트는 5 
fit <- with(imp,lm(date.mean~., data=holicli))
pooled <- pool(fit)       # pooled 는 m개의 분석결과의 평균이다               
summary(pooled)
imp
newvalues = imp$imp$date.mean; newvalues   # 각 row마다 5번씩 구해진 값 나열
finalvalues = rowMeans(newvalues)   # 평균을 내서 최종 값을 구하자!

# replacing wrong values with the predicted ones
original <- read.csv("(mean)water_holi_cli_home.csv",header=T)
View(original)
original[430, 2] = finalvalues[1]
original[433:440, 2] = finalvalues[2:9]

write.csv(original, "Semifinal_ReplacedWithMICE.csv", row.names=F)
