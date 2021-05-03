install.packages("arules")

library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(hrbrthemes)
library(arules)


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

#Discretize data
alt_happy <- total_means %>% select(Country,Score.2015,Score.2016,Score.2017,Score.2018,Score.2019)
alt_happy.long <- melt(alt_happy,id.vars ="Country")
alt_happy.long <- alt_happy.long %>% rename("Happiness" = value)
alt_happy <- select(alt_happy.long,Happiness)

alt_gdp <- total_means %>% select(Country,GDP.2015,GDP.2016,GDP.2017,GDP.2018,GDP.2019)
alt_gdp.long <- melt(alt_gdp,id.vars ="Country")
alt_gdp.long <- alt_gdp.long %>% rename("GDP" = value)
alt_gdp <- select(alt_gdp.long,GDP)

alt_social <- total_means %>% select(Country,Social.2015,Social.2016,Social.2017,Social.2018,Social.2019)
alt_social.long <- melt(alt_social,id.vars ="Country")
alt_social.long <- alt_social.long %>% rename("Social" = value)
alt_social <- select(alt_social.long,Social)

alt_health <- total_means %>% select(Country,Health.2015,Health.2016,Health.2017,Health.2018,Health.2019)
alt_health.long <- melt(alt_health,id.vars ="Country")
alt_health.long <- alt_health.long %>% rename("Health" = value)
alt_health <- select(alt_health.long,Health)

alt_Freedom <- total_means %>% select(Country,Freedom.2015,Freedom.2016,Freedom.2017,Freedom.2018,Freedom.2019)
alt_Freedom.long <- melt(alt_Freedom,id.vars ="Country")
alt_Freedom.long <- alt_Freedom.long %>% rename("Freedom" = value)
alt_Freedom <- select(alt_Freedom.long,Freedom)

alt_generosity <- total_means %>% select(Country,Generosity.2015,Generosity.2016,Generosity.2017,Generosity.2018,Generosity.2019)
alt_generosity.long <- melt(alt_generosity,id.vars ="Country")
alt_generosity.long <- alt_generosity.long %>% rename("Generosity" = value)
alt_generosity <- select(alt_generosity.long,Generosity)

alt_corruption <- total_means %>% select(Country,Perceptions.of.corruption.2015,Perceptions.of.corruption.2016,Perceptions.of.corruption.2017,Perceptions.of.corruption.2018,Perceptions.of.corruption.2019)
alt_corruption.long <- melt(alt_corruption,id.vars ="Country")
alt_corruption.long <- alt_corruption.long %>% rename("Corruption" = value)
alt_corruption <- select(alt_corruption.long,Corruption)

alt_data <- cbind(alt_happy, alt_gdp)
alt_data <- cbind(alt_data, alt_social)
alt_data <- cbind(alt_data, alt_health)
alt_data <- cbind(alt_data, alt_Freedom)
alt_data <- cbind(alt_data, alt_generosity)

alt_data <- cbind(alt_data, alt_corruption)

is.numeric(alt_Freedom[1,1])
is.numeric(alt_corruption[1,1])

#Predictions


alt_predict <- alt_data
testidx <- which(1:nrow(alt_predict)%%2==0)
predict_data <- alt_predict[-testidx,]
predict_test <- alt_predict[testidx,]

alt_model <- lm(formula = Happiness  ~  GDP + Social + Health + Freedom + Generosity + as.numeric(Corruption), data = predict_data)
prediction <- predict(alt_model, newdata=predict_test)
cor(prediction, predict_test$Happiness)
summary(alt_model)
###
predict_means <- total_means %>% select(Score.Mean, GDP.Mean, Social.support.Mean, Health.Mean, Freedom.Mean, Generosity.Mean, Perceptions.of.corruption.Mean)
testidx <- which(1:nrow(predict_means)%%4==0)
happy_data <- predict_means[-testidx,]
happy_test <- predict_means[testidx,]


model <- lm(Score.Mean~., data=happy_data)
prediction <- predict(model, newdata=happy_test)
cor(prediction, happy_test$Score.Mean)
summary(model)



Happy.All <- alt_happy.long[,3]

def.par <- par(no.readonly = TRUE)
layout(mat = rbind(1:2,3:4))


table(discretize(Happy.All, breaks = 5))
geom_bar(Happy.All, breaks = 20, main = "Equal Frequency")
abline(v = discretize(Happy.All, breaks = 5, labels = FALSE,
                      onlycuts = TRUE), col = "red")