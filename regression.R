library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(hrbrthemes)

#Prepaing Data
predict_means <- total_means %>% select(Score.Mean, GDP.Mean, Social.support.Mean, Health.Mean, Freedom.Mean, Generosity.Mean, Perceptions.of.corruption.Mean)
testidx <- which(1:nrow(predict_means)%%4==0)
happy_data <- predict_means[-testidx,]
happy_test <- predict_means[testidx,]

#Model Linear Regression
model <- lm(Score.Mean~., data=happy_data)

prediction <- predict(model, newdata=happy_test)

cor(prediction, happy_test$Score.Mean)

summary(model)