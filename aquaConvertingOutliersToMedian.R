usageDatInfo = read.csv(file.choose(), header = T)

usageDatInfo = usageDatInfo[,2:30]
usageDatInfo = usageDatInfo[,-27]

levels(usageDatInfo$type)

usageDatInfoGeneral = usageDatInfo[usageDatInfo$type=='general',]
usageDatInfoHome = usageDatInfo[usageDatInfo$type=='home',]
usageDatInfoSchool = usageDatInfo[usageDatInfo$type=='school',]

#for function
# x = usageDatInfoSchool
# x[,-c(3:26)] = 0
# dim(x)
# 
# matX = matrix(0, dim(x)[1],dim(x)[2])
# for ( i in 1:dim(x)[1])
# {
#   for ( j in 1:dim(x)[2])
#   {
#     matX[i,j] = paste(i,j,sep=',')
#   }
# }
# 
# coordinateOfTheOutOfCriterion = matX[x>2429]
# 
# for ( i in coordinateOfTheOutOfCriterion )
# {
#   xyAxis = strsplit(a,split=',')
#   xAxis = as.numeric(xyAxis[[1]][1])
#   yAxis = as.numeric(xyAxis[[1]][2])
#   
#   date = usageDatInfoSchool[xAxis,2]
#   sameDateHour = usageDatInfoSchool[usageDatInfoSchool[,2]==date,yAxis]
#   x[xAxis,yAxis] = median(sameDateHour)
# }
# 
# #usageDatInfoSchool[xAxis,2] #며칠
# #colnames(usageDatInfo)[yAxis] #몇시?





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

#1201 5445
#usageDatInfoGeneral = putMedianIn(usageDatInfoGeneral,1)
#usageDatInfoSchool = putMedianIn(usageDatInfoSchool,1)
#usageDatInfoHome = putMedianIn(usageDatInfoHome,1)

# ?write.csv
# write.csv(usageDatInfoGeneral, file = "usageDatInfoGeneral.csv")
# write.csv(usageDatInfoSchool, file = "usageDatInfoSchool.csv")
# write.csv(usageDatInfoHome, file = "usageDatInfoHome.csv")
