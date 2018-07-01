library(dplyr)
home<-read.csv('usageDatInfoHome.csv')
View(home)
home <- home[,-1]
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
home.day.mean$ym <- as.numeric(format(home.day.mean$Data, "%Y%m"))
View(home.day.mean)

cli.mean<-read.csv('cli.new.csv')
cli.mean$date <- anydate(cli.mean$date)
home.day.newmean <- full_join(home.day.mean,cli.mean,by=c('Data'='date')) 
home.day.newmean$rain <- ifelse(home.day.newmean$rain==0,0,1)
home.day.newmean <- home.day.newmean[-c(456,457),]

View(home.day.newmean)

home.day.newmean$high.temp <- NULL
home.day.newmean$low.temp <- NULL
home.day.newmean$ym <- NULL

View(home.day.newmean)

write.csv(home.day.newmean, "(mean)water_holiday_climate.csv", row.names=F)
