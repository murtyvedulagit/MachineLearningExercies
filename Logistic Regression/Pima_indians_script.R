# Pima Indians Diabetes Prediction Model - Logistic Regression

# Read Data
pimaindian.df <- read.csv(file = "Logistic Regression/pima_indians_diabetes_study/Pima Indians Diabetes Binary Classification dataset.csv", header = T, )

#change long column names of calss variable
names(pimaindian.df)[names(pimaindian.df) == 'Class.variable..0.or.1.'] <- 'class.variable1'

#Split data into Train and Test
library(caTools)

set.seed(99)
splitvar <- sample.split(pimaindian.df$class.variable1, SplitRatio = 0.7)
train <- subset(pimaindian.df, splitvar = TRUE)
test  <- subset(pimaindian.df, splitvar = FALSE)

#Apply Logistic regression

pimaglm <- glm(class.variable1 ~., family = binomial(link = "logit"), data = train)
summary(pimaglm)
predicted.values <- predict(pimaglm, newdata = test, type = "response")
predicted.values <- ifelse( predicted.values > 0.5, 1, 0)
print(table(predicted.values, test$class.variable1))

