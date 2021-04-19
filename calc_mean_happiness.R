library(ggplot2)
library(plyr)
library(dplyr)

##Merge data and calculate mean happiness score

##2015
new_2015 <- Happy2015 %>% 
  select(Country, Happiness.Score)
new_2015 <- new_2015 %>% rename(Score.2015 = Happiness.Score, , Country.or.region = Country)

##2016
new_2016 <- Happy2016 %>% 
  select(Country, Happiness.Score)
new_2016 <- new_2016 %>% rename(Score.2016 = Happiness.Score, , Country.or.region = Country)

##2017
new_2017 <- Happy2017 %>% 
  select(Country, Happiness.Score)
new_2017 <- new_2017 %>% rename(Score.2017 = Happiness.Score, Country.or.region = Country)

##2018
new_2018 <- Happy2018 %>% 
  select(Country.or.region, Score)
new_2018 <- new_2018 %>% rename(Score.2018 = Score)

##2019
new_2019 <- Happy2019 %>% 
  select(Country.or.region, Score)
new_2019 <- new_2019 %>% rename(Score.2019 = Score)


##Merge data
mean_happiness <- merge(new_2015, new_2016)
mean_happiness <- merge(mean_happiness, new_2017)
mean_happiness <- merge(mean_happiness, new_2018)
mean_happiness <- merge(mean_happiness, new_2019)


##Calculate mean happiness
mean_happiness <- mean_happiness %>%
  rowwise() %>%
  mutate(Score.Mean = mean(Score.2015,Score.2016,Score.2017,Score.2018,Score.2019))

mean_happiness <- arrange(mean_happiness, desc(Score.Mean))

head(mean_happiness, 10)
