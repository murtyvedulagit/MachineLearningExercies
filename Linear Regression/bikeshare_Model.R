#Bike Share Project on Kaggle
bike <- read.csv('Linear Regression/bikeshare.csv')
#Exploratory Data Analysis
library(ggplot2)
library(dplyr)
#print(ggplot(bike, aes(temp,count)) + geom_point(alpha =0.3, aes(color=temp)) + theme_bw())
#convert dttm to POSIXCT
bike$datetime <- as.POSIXct(bike$datetime)
#feature engineering
bike$hour <- sapply(bike$datetime, function(x) {format(x, "%H")})
bike$hour <- sapply(bike$hour, as.numeric)


#Build Model
model <- lm(count~. - casual - registered - datetime - atemp, bike)
print(summary(model))

