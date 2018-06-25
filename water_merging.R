getwd()
setwd("/Users/Dana/Desktop/ONGOING/water/data")
library(ggplot2)
library(tidyr)
install.packages("dplyr")
library(dplyr)
library(plyr)

originaldata <- read.csv("water_usage_dataset_new.csv",header=T)
customerdata <- read.csv("cus_new.csv",header=T)
weatherdata <- read.csv("weather.csv",header=T)

head(customerdata)
head(originaldata)
originaldata$Meter_ID = "meter_id"

merged <- full_join(originaldata,customerdata,by=c("meter_id"='meter_id'))
write.csv(merged,file="customer_and_waterusage_merged.csv")


