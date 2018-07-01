getwd()
setwd("/Users/Dana/Desktop/ONGOING/water/data")
install.packages("car")
library(car)
install.packages('anytime')
library(anytime)

dat <- read.csv("_home_water_holi_cli.csv",header=T)
View(dat)
head(dat)

# factorizing and removing unnecessary columns
dat$holiday <- factor(dat$holiday)
dat$new_thanks <- factor(dat$new_thanks)
dat$rain <- factor(dat$rain)
dat$Data <- NULL
dat$X <- NULL

# remove rows #433~439 for prediction
dat1 = dat[1:432, ]
dat2 = dat[440:457, ]
newdat = rbind(dat1,dat2)

# fitting regression
fit<-lm(date.sum~. -high.temp -low.temp ,data=newdat)
summary(fit)
vif(fit)

# predict
dat3 = dat[433:439,]; dat3
prediction <- predict(fit, newdata = dat3)
prediction

# replacing wrong values with the predicted ones
original <- read.csv("_home_water_holi_cli.csv",header=T)
original[433:439,3] = prediction
original[433:439,3]

# export
write.csv(original, "replaced_with_regressed_values.csv", row.names=F)
