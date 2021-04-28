library(ggplot2)
library(plyr)
library(dplyr)

  ###################################################
 ## Merge data and calculate mean happiness score ##
###################################################

Happy2015 <- as.data.frame(read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\2015.csv'))
Happy2016 <- as.data.frame(read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\2016.csv'))
Happy2017 <- as.data.frame(read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\2017.csv'))
Happy2018 <- as.data.frame(read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\2018.csv'))
Happy2019 <- as.data.frame(read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\2019.csv'))

##2015
happy_2015 <- Happy2015 %>% 
  select(Country, Score)
happy_2015 <- happy_2015 %>% rename(Score.2015 = Score)

##2016
happy_2016 <- Happy2016 %>% 
  select(Country, Score)
happy_2016 <- happy_2016 %>% rename(Score.2016 = Score)

##2017
happy_2017 <- Happy2017 %>% 
  select(Country, Score)
happy_2017 <- happy_2017 %>% rename(Score.2017 = Score)

##2018
happy_2018 <- Happy2018 %>% 
  select(Country, Score)
happy_2018 <- happy_2018 %>% rename(Score.2018 = Score)

##2019
happy_2019 <- Happy2019 %>% 
  select(Country, Score)
happy_2019 <- happy_2019 %>% rename(Score.2019 = Score)


##Merge data
mean_happiness <- merge(happy_2015, happy_2016)
mean_happiness <- merge(mean_happiness, happy_2017)
mean_happiness <- merge(mean_happiness, happy_2018)
mean_happiness <- merge(mean_happiness, happy_2019)


##Calculate mean happiness
mean_happiness <- mean_happiness %>%
  rowwise() %>%
  mutate(Score.Mean = mean(Score.2015,Score.2016,Score.2017,Score.2018,Score.2019))

mean_happiness <- arrange(mean_happiness, desc(Score.Mean))

head(mean_happiness, 10)


  ########################################################
 ## Merge data and calculate mean GDP per Capita score ##
########################################################

##2015
gdp_2015 <- Happy2015 %>% 
  select(Country, GDP.per.capita)
gdp_2015 <- gdp_2015 %>% rename(GDP.2015 = GDP.per.capita)

##2016
gdp_2016 <- Happy2016 %>% 
  select(Country, GDP.per.capita)
gdp_2016 <- gdp_2016 %>% rename(GDP.2016 = GDP.per.capita)

##2017
gdp_2017 <- Happy2017 %>% 
  select(Country, GDP.per.capita)
gdp_2017 <- gdp_2017 %>% rename(GDP.2017 = GDP.per.capita)

##2018
gdp_2018 <- Happy2018 %>% 
  select(Country, GDP.per.capita)
gdp_2018 <- gdp_2018 %>% rename(GDP.2018 = GDP.per.capita)

##2019
gdp_2019 <- Happy2019 %>% 
  select(Country, GDP.per.capita)
gdp_2019 <- gdp_2019 %>% rename(GDP.2019 = GDP.per.capita)


##Merge data
mean_GDP <- merge(gdp_2015, gdp_2016)
mean_GDP <- merge(mean_GDP, gdp_2017)
mean_GDP <- merge(mean_GDP, gdp_2018)
mean_GDP <- merge(mean_GDP, gdp_2019)


##Calculate mean GDP
mean_GDP <- mean_GDP %>%
  rowwise() %>%
  mutate(GDP.Mean = mean(GDP.2015,GDP.2016,GDP.2017,GDP.2018,GDP.2019))

mean_GDP <- arrange(mean_GDP, desc(GDP.Mean))

head(mean_GDP, 10)


  #########################################################
 ## Merge data and calculate mean Social Support score ##
#######################################################

##2015
Social_2015 <- Happy2015 %>% 
  select(Country, Social.support)
Social_2015 <- Social_2015 %>% rename(Social.2015 = Social.support)

##2016
Social_2016 <- Happy2016 %>% 
  select(Country, Social.support)
Social_2016 <- Social_2016 %>% rename(Social.2016 = Social.support)

##2017
Social_2017 <- Happy2017 %>% 
  select(Country, Social.support)
Social_2017 <- Social_2017 %>% rename(Social.2017 = Social.support)

##2018
Social_2018 <- Happy2018 %>% 
  select(Country, Social.support)
Social_2018 <- Social_2018 %>% rename(Social.2018 = Social.support)

##2019
Social_2019 <- Happy2019 %>% 
  select(Country, Social.support)
Social_2019 <- Social_2019 %>% rename(Social.2019 = Social.support)


##Merge data
mean_Social <- merge(Social_2015, Social_2016)
mean_Social <- merge(mean_Social, Social_2017)
mean_Social <- merge(mean_Social, Social_2018)
mean_Social <- merge(mean_Social, Social_2019)


##Calculate mean Social Support
mean_Social.support <- mean_Social %>%
  rowwise() %>%
  mutate(Social.support.Mean = mean(Social.2015,Social.2016,Social.2017,Social.2018,Social.2019))

mean_Social.support <- arrange(mean_Social.support, desc(Social.support.Mean))

head(mean_Social.support, 10)


  ####################################################################
 ## Merge data and calculate mean Health and Life Expectancy score ##
####################################################################

##2015
Health_2015 <- Happy2015 %>% 
  select(Country, Health..Life.Expectancy.)
Health_2015 <- Health_2015 %>% rename(Health.2015 = Health..Life.Expectancy.)

##2016
Health_2016 <- Happy2016 %>% 
  select(Country, Health..Life.Expectancy.)
Health_2016 <- Health_2016 %>% rename(Health.2016 = Health..Life.Expectancy.)

##2017
Health_2017 <- Happy2017 %>% 
  select(Country, Health..Life.Expectancy.)
Health_2017 <- Health_2017 %>% rename(Health.2017 = Health..Life.Expectancy.)

##2018
Health_2018 <- Happy2018 %>% 
  select(Country, Health..Life.Expectancy.)
Health_2018 <- Health_2018 %>% rename(Health.2018 = Health..Life.Expectancy.)

##2019
Health_2019 <- Happy2019 %>% 
  select(Country, Health..Life.Expectancy.)
Health_2019 <- Health_2019 %>% rename(Health.2019 = Health..Life.Expectancy.)


##Merge data
mean_Health <- merge(Health_2015, Health_2016)
mean_Health <- merge(mean_Health, Health_2017)
mean_Health <- merge(mean_Health, Health_2018)
mean_Health <- merge(mean_Health, Health_2019)


##Calculate mean Health and Life Expectancy
mean_Health <- mean_Health %>%
  rowwise() %>%
  mutate(Health.Mean = mean(Health.2015,Health.2016,Health.2017,Health.2018,Health.2019))

mean_Health <- arrange(mean_Health, desc(Health.Mean))

head(mean_Health, 10)


#####################################################
## Merge data and calculate mean Freedom score ##
####################################################

##2015
Freedom_2015 <- Happy2015 %>% 
  select(Country, Freedom)
Freedom_2015 <- Freedom_2015 %>% rename(Freedom.2015 = Freedom)

##2016
Freedom_2016 <- Happy2016 %>% 
  select(Country, Freedom)
Freedom_2016 <- Freedom_2016 %>% rename(Freedom.2016 = Freedom)

##2017
Freedom_2017 <- Happy2017 %>% 
  select(Country, Freedom)
Freedom_2017 <- Freedom_2017 %>% rename(Freedom.2017 = Freedom)

##2018
Freedom_2018 <- Happy2018 %>% 
  select(Country, Freedom)
Freedom_2018 <- Freedom_2018 %>% rename(Freedom.2018 = Freedom)

##2019
Freedom_2019 <- Happy2019 %>% 
  select(Country, Freedom)
Freedom_2019 <- Freedom_2019 %>% rename(Freedom.2019 = Freedom)


##Merge data
mean_Freedom <- merge(Freedom_2015, Freedom_2016)
mean_Freedom <- merge(mean_Freedom, Freedom_2017)
mean_Freedom <- merge(mean_Freedom, Freedom_2018)
mean_Freedom <- merge(mean_Freedom, Freedom_2019)


##Calculate mean Generosity
mean_Generosity <- mean_Generosity %>%
  rowwise() %>%
  mutate(Generosity.Mean = mean(Generosity.2015,Generosity.2016,Generosity.2017,Generosity.2018,Generosity.2019))

mean_Generosity <- arrange(mean_Generosity, desc(Generosity.Mean))

head(mean_Generosity, 10)

  #####################################################
 ## Merge data and calculate mean Generosity score ##
####################################################

##2015
Generosity_2015 <- Happy2015 %>% 
  select(Country, Generosity)
Generosity_2015 <- Generosity_2015 %>% rename(Generosity.2015 = Generosity)

##2016
Generosity_2016 <- Happy2016 %>% 
  select(Country, Generosity)
Generosity_2016 <- Generosity_2016 %>% rename(Generosity.2016 = Generosity)

##2017
Generosity_2017 <- Happy2017 %>% 
  select(Country, Generosity)
Generosity_2017 <- Generosity_2017 %>% rename(Generosity.2017 = Generosity)

##2018
Generosity_2018 <- Happy2018 %>% 
  select(Country, Generosity)
Generosity_2018 <- Generosity_2018 %>% rename(Generosity.2018 = Generosity)

##2019
Generosity_2019 <- Happy2019 %>% 
  select(Country, Generosity)
Generosity_2019 <- Generosity_2019 %>% rename(Generosity.2019 = Generosity)


##Merge data
mean_Generosity <- merge(Generosity_2015, Generosity_2016)
mean_Generosity <- merge(mean_Generosity, Generosity_2017)
mean_Generosity <- merge(mean_Generosity, Generosity_2018)
mean_Generosity <- merge(mean_Generosity, Generosity_2019)


##Calculate mean Generosity
mean_Generosity <- mean_Generosity %>%
  rowwise() %>%
  mutate(Generosity.Mean = mean(Generosity.2015,Generosity.2016,Generosity.2017,Generosity.2018,Generosity.2019))

mean_Generosity <- arrange(mean_Generosity, desc(Generosity.Mean))

head(mean_Generosity, 10)


  ###################################################################
 ## Merge data and calculate mean Perceptions of Corruption score ##
###################################################################

##2015
Perceptions.of.corruption_2015 <- Happy2015 %>% 
  select(Country, Perceptions.of.corruption)
Perceptions.of.corruption_2015 <- Perceptions.of.corruption_2015 %>% rename(Perceptions.of.corruption.2015 = Perceptions.of.corruption)

##2016
Perceptions.of.corruption_2016 <- Happy2016 %>% 
  select(Country, Perceptions.of.corruption)
Perceptions.of.corruption_2016 <- Perceptions.of.corruption_2016 %>% rename(Perceptions.of.corruption.2016 = Perceptions.of.corruption)

##2017
Perceptions.of.corruption_2017 <- Happy2017 %>% 
  select(Country, Perceptions.of.corruption)
Perceptions.of.corruption_2017 <- Perceptions.of.corruption_2017 %>% rename(Perceptions.of.corruption.2017 = Perceptions.of.corruption)

##2018
Perceptions.of.corruption_2018 <- Happy2018 %>% 
  select(Country, Perceptions.of.corruption)
Perceptions.of.corruption_2018 <- Perceptions.of.corruption_2018 %>% rename(Perceptions.of.corruption.2018 = Perceptions.of.corruption)

##2019
Perceptions.of.corruption_2019 <- Happy2019 %>% 
  select(Country, Perceptions.of.corruption)
Perceptions.of.corruption_2019 <- Perceptions.of.corruption_2019 %>% rename(Perceptions.of.corruption.2019 = Perceptions.of.corruption)


##Merge data
mean_Perceptions.of.corruption <- merge(Perceptions.of.corruption_2015, Perceptions.of.corruption_2016)
mean_Perceptions.of.corruption <- merge(mean_Perceptions.of.corruption, Perceptions.of.corruption_2017)
mean_Perceptions.of.corruption <- merge(mean_Perceptions.of.corruption, Perceptions.of.corruption_2018)
mean_Perceptions.of.corruption <- merge(mean_Perceptions.of.corruption, Perceptions.of.corruption_2019)


##Calculate mean Perceptions of Corruption
mean_Perceptions.of.corruption <- mean_Perceptions.of.corruption %>%
  rowwise() %>%
  mutate(Perceptions.of.corruption.Mean = mean(Perceptions.of.corruption.2015,Perceptions.of.corruption.2016,Perceptions.of.corruption.2017,Perceptions.of.corruption.2018,Perceptions.of.corruption.2019))

mean_Perceptions.of.corruption <- arrange(mean_Perceptions.of.corruption, desc(Perceptions.of.corruption.Mean))

head(mean_Perceptions.of.corruption, 10)


############################
## Display all mean data ##
###########################

total_means <- merge(mean_happiness, mean_GDP)
total_means <- merge(total_means, mean_Social)
total_means <- merge(total_means, mean_Health)
total_means <- merge(total_means, mean_Freedom)
total_means <- merge(total_means, mean_Generosity)
total_means <- merge(total_means, mean_Perceptions.of.corruption)

total_means <- total_means %>% arrange(desc(Score.Mean))

# Save the averaged data

write.csv(total_means, 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\Averaged_Data.csv')

