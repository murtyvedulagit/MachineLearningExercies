#extrat the student performance data from the csv file
df <- read.csv('Linear Regression/student-mat.csv', sep = ';')
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)
#numeric columns only
num.cols <- sapply(df, is.numeric)
#filter correlation data
cor.data <- cor(df[, num.cols])
#print(cor.data)
#print(corrplot(cor.data, method = 'color'))
#print(corrgram(df, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt))
print(ggplot(df, aes(x=G3))+geom_histogram(bins=20))
#Set a seed
set.seed(101)
#Split the data into Train and Test
sample <- sample.split(df$G3, SplitRatio = 0.7)
#70% of data is training data
train <- subset(df, sample==T)
#30% of data is testing data
test <- subset(df, sample==F)
#train and build the lin reg model
model <- lm(G3~., data=train)
#print(summary(model))
#res <- residuals(model)
#res <- as.data.frame(res)
#print(ggplot(res, aes(res)) +geom_histogram(fill = 'blue'))
#stats of plot
#print(plot(model))

G3.predictions <- predict(model, test)
results <- cbind(G3.predictions, test$G3)
results <- as.data.frame(results)
colnames(results) <- c('predicted', 'actual')
#print(head(results))
#remove negative predictions
to_zero <- function(x){
  if (x< 0) {
    return (0)} 
  else {
    return (x)}
  }
#apply to results
results$predicted <- sapply(results$predicted, to_zero)


##Mean sqaured error
mse <- mean ((results$actual - results$predicted)^2)

SSE <- sum( (results$actual - results$predicted)^2)
SST <- sum( (mean(df$G3) - results$actual)^2)
R2 <- 1 - SSE/SST
print("R2 is")
print(R2)



