#Bike Share Project on Kaggle
bike <- read.csv('Linear Regression/bikeshare.csv')
#Exploratory Data Analysis
library(ggplot2)
library(dplyr)
#print(ggplot(bike, aes(temp,count)) + geom_point(alpha =0.3, aes(color=temp)) + theme_bw())
#convert dttm to POSIXCT
bike$datetime <- as.POSIXct(bike$datetime)
pl <- ggplot(bike, aes(datetime,count)) + geom_point(aes(color=temp))
pl + scale_color_continuous(low = '#55DbCE', high = '#FF6E2E') +theme_bw()
#print(pl)
pl2 <- ggplot(bike, aes(factor(season), count)) + geom_boxplot(aes(color = factor(season))) + theme_bw()
#print(pl2)
#feature engineering
bike$hour <- sapply(bike$datetime, function(x) {format(x, "%H")})
pl3 <- ggplot(filter(bike, workingday==0), aes(hour, count))
pl3 <- pl3 + geom_point(aes(color=temp), alpha = 0.5)
pl3 <- pl3 + scale_color_gradientn(colours = c('dark blue', 'blue', 'light blue', 'light green', 'yellow', 'orange', 'red'))
print(pl3)
