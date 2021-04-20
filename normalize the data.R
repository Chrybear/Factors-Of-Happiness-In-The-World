# Normalizing the data to be easier to merge later

library(dplyr)
library(ggplot2)

# Load up our data
twenty_fifteen <- as.data.frame(read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\2015.csv'))
twenty_sixteen <- as.data.frame(read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\2016.csv'))
twenty_seventeen <- as.data.frame(read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\2017.csv'))
twenty_eighteen <- as.data.frame(read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\2018.csv'))
twenty_nineteen <- as.data.frame(read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\2019.csv'))

# Make the data homogeneous

# Country
twenty_eighteen <- rename(twenty_eighteen, "Country" = "Country.or.region")
twenty_nineteen <- rename(twenty_nineteen, "Country" = "Country.or.region")

# Score
twenty_fifteen <- rename(twenty_fifteen, "Score" = "Happiness.Score")
twenty_seventeen <- rename(twenty_seventeen, "Score" = "Happiness.Score")
twenty_sixteen <- rename(twenty_sixteen, "Score" = "Happiness.Score")

# GDP
twenty_fifteen <- rename(twenty_fifteen, "GDP.per.capita" = "Economy..GDP.per.Capita.")
twenty_seventeen <- rename(twenty_seventeen, "GDP.per.capita" = "Economy..GDP.per.Capita.")
twenty_sixteen <- rename(twenty_sixteen, "GDP.per.capita" = "Economy..GDP.per.Capita.")

# Life expectancy
twenty_eighteen <- rename(twenty_eighteen, "Health..Life.Expectancy" = "Healthy.life.expectancy")
twenty_nineteen <- rename(twenty_nineteen, "Health..Life.Expectancy" = "Healthy.life.expectancy")

# Corruption
twenty_fifteen <- rename(twenty_fifteen, "Perceptions.of.corruption" = "Trust..Government.Corruption.")
twenty_seventeen <- rename(twenty_seventeen, "Perceptions.of.corruption" = "Trust..Government.Corruption.")
twenty_sixteen <- rename(twenty_sixteen, "Perceptions.of.corruption" = "Trust..Government.Corruption.")

# freedom
twenty_eighteen <- rename(twenty_eighteen, "Freedom" = "Freedom.to.make.life.choices")
twenty_nineteen <- rename(twenty_nineteen, "Freedom" = "Freedom.to.make.life.choices")

# Social Support = Family?
twenty_fifteen <- rename(twenty_fifteen, "Social.support" = "Family")
twenty_seventeen <- rename(twenty_seventeen, "Social.support" = "Family")
twenty_sixteen <- rename(twenty_sixteen, "Social.support" = "Family")


# Write them
write.csv(twenty_fifteen, 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\2015.csv')
write.csv(twenty_sixteen, 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\2016.csv')
write.csv(twenty_seventeen, 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\2017.csv')
write.csv(twenty_eighteen, 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\2018.csv')
write.csv(twenty_nineteen, 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\2019.csv')




