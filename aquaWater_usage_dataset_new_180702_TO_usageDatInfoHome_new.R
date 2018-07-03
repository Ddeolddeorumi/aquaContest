library(dplyr)

originaldata <- read.csv(file.choose(),header=T) #water_usage_dataset_new_180702.csv
customerdata <- read.csv(file.choose(),header=T) #customer_info(최종).csv
weatherdata <- read.csv(file.choose(),header=T) #weatherConditionInfo.csv

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
