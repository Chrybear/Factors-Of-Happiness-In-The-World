library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(hrbrthemes)

# Find which countries had the biggest overall improvement and decline in happiness from 2015 to 2019

avg_happy <- read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\Averaged_Data.csv')

big_happy <- avg_happy %>% arrange(desc(Score.2019 - Score.2015))

big_sad <- avg_happy %>% arrange(Score.2019 - Score.2015)

# This is redundant; could just use tail on big_happy for big_sad, but I like the silly names
# head(big_happy,1)
# head(big_sad,1)

# Biggest increase: Benin
# Biggest decrease: Venezuela

big_happy <- head(big_happy, 1)
big_sad <- head(big_sad, 1)

# Show all value changes over the 5 years for Benin (big_happy)

# Get each year's values for Benin
benin_vals_2015 <- big_happy %>% select(Score.2015, GDP.2015, Social.2015, Health.2015, Freedom.2015, Generosity.2015, Perceptions.of.corruption.2015)
benin_vals_2016 <- big_happy %>% select(Score.2016, GDP.2016, Social.2016, Health.2016, Freedom.2016, Generosity.2016, Perceptions.of.corruption.2016)
benin_vals_2017 <- big_happy %>% select(Score.2017, GDP.2017, Social.2017, Health.2017, Freedom.2017, Generosity.2017, Perceptions.of.corruption.2017)
benin_vals_2018 <- big_happy %>% select(Score.2018, GDP.2018, Social.2018, Health.2018, Freedom.2018, Generosity.2018, Perceptions.of.corruption.2018)
benin_vals_2019 <- big_happy %>% select(Score.2019, GDP.2019, Social.2019, Health.2019, Freedom.2019, Generosity.2019, Perceptions.of.corruption.2019)
# Make the data look nicer
# 2015
colnames(benin_vals_2015)[1] <- "Score"
colnames(benin_vals_2015)[2] <- "GDP"
colnames(benin_vals_2015)[3] <- "Welfare"
colnames(benin_vals_2015)[4] <- "Health"
colnames(benin_vals_2015)[5] <- "Freedom"
colnames(benin_vals_2015)[6] <- "Generosity"
colnames(benin_vals_2015)[7] <- "Corruption"
# 2016
colnames(benin_vals_2016)[1] <- "Score"
colnames(benin_vals_2016)[2] <- "GDP"
colnames(benin_vals_2016)[3] <- "Welfare"
colnames(benin_vals_2016)[4] <- "Health"
colnames(benin_vals_2016)[5] <- "Freedom"
colnames(benin_vals_2016)[6] <- "Generosity"
colnames(benin_vals_2016)[7] <- "Corruption"
# 2017
colnames(benin_vals_2017)[1] <- "Score"
colnames(benin_vals_2017)[2] <- "GDP"
colnames(benin_vals_2017)[3] <- "Welfare"
colnames(benin_vals_2017)[4] <- "Health"
colnames(benin_vals_2017)[5] <- "Freedom"
colnames(benin_vals_2017)[6] <- "Generosity"
colnames(benin_vals_2017)[7] <- "Corruption"
# 2018
colnames(benin_vals_2018)[1] <- "Score"
colnames(benin_vals_2018)[2] <- "GDP"
colnames(benin_vals_2018)[3] <- "Welfare"
colnames(benin_vals_2018)[4] <- "Health"
colnames(benin_vals_2018)[5] <- "Freedom"
colnames(benin_vals_2018)[6] <- "Generosity"
colnames(benin_vals_2018)[7] <- "Corruption"
# 2019
colnames(benin_vals_2019)[1] <- "Score"
colnames(benin_vals_2019)[2] <- "GDP"
colnames(benin_vals_2019)[3] <- "Welfare"
colnames(benin_vals_2019)[4] <- "Health"
colnames(benin_vals_2019)[5] <- "Freedom"
colnames(benin_vals_2019)[6] <- "Generosity"
colnames(benin_vals_2019)[7] <- "Corruption"





#Added Year to data
benin_vals_2015 <- mutate(benin_vals_2015, Year = 2015)
benin_vals_2016 <- mutate(benin_vals_2016, Year = 2016)
benin_vals_2017 <- mutate(benin_vals_2017, Year = 2017)
benin_vals_2018 <- mutate(benin_vals_2018, Year = 2018)
benin_vals_2019 <- mutate(benin_vals_2019, Year = 2019)

benin_vals_2015.long <- melt(benin_vals_2015,id.vars ="Year")
benin_vals_2016.long <- melt(benin_vals_2016,id.vars ="Year")
benin_vals_2017.long <- melt(benin_vals_2017,id.vars ="Year")
benin_vals_2018.long <- melt(benin_vals_2018,id.vars ="Year")
benin_vals_2019.long <- melt(benin_vals_2019,id.vars ="Year")


#Recombined Data to allow graphing
benin_vals.long <- rbind(benin_vals_2015.long, benin_vals_2016.long)
benin_vals.long <- rbind(benin_vals.long, benin_vals_2017.long)
benin_vals.long <- rbind(benin_vals.long, benin_vals_2018.long)
benin_vals.long <- rbind(benin_vals.long, benin_vals_2019.long)

#Makes a beautiful graph
ggplot(benin_vals.long, aes(x=variable, y = value, fill=factor(Year)))+
  geom_bar(stat = "identity", position = "dodge") + xlab(NULL) + ylab("Value Score") +
  ggtitle("Changes For Benin From 2015-2019") + theme(plot.title = element_text(hjust = 0.5, size = 24), axis.text.y=element_blank(), axis.text.x =element_text(size = 15)) +
  geom_text(aes(label=(sprintf("%0.3f", round(as.double(value), digits = 3)))), position = position_dodge(width=0.9), vjust=1.5) +
  scale_fill_discrete(name = "Year")





# Get each year's values for Venezuela
ven_vals_2015 <- big_sad %>% select(Score.2015, GDP.2015, Social.2015, Health.2015, Freedom.2015, Generosity.2015, Perceptions.of.corruption.2015)
ven_vals_2016 <- big_sad %>% select(Score.2016, GDP.2016, Social.2016, Health.2016, Freedom.2016, Generosity.2016, Perceptions.of.corruption.2016)
ven_vals_2017 <- big_sad %>% select(Score.2017, GDP.2017, Social.2017, Health.2017, Freedom.2017, Generosity.2017, Perceptions.of.corruption.2017)
ven_vals_2018 <- big_sad %>% select(Score.2018, GDP.2018, Social.2018, Health.2018, Freedom.2018, Generosity.2018, Perceptions.of.corruption.2018)
ven_vals_2019 <- big_sad %>% select(Score.2019, GDP.2019, Social.2019, Health.2019, Freedom.2019, Generosity.2019, Perceptions.of.corruption.2019)

# Make the data look nicer
# 2015
colnames(ven_vals_2015)[1] <- "Score"
colnames(ven_vals_2015)[2] <- "GDP"
colnames(ven_vals_2015)[3] <- "Welfare"
colnames(ven_vals_2015)[4] <- "Health"
colnames(ven_vals_2015)[5] <- "Freedom"
colnames(ven_vals_2015)[6] <- "Generosity"
colnames(ven_vals_2015)[7] <- "Corruption"
# 2016
colnames(ven_vals_2016)[1] <- "Score"
colnames(ven_vals_2016)[2] <- "GDP"
colnames(ven_vals_2016)[3] <- "Welfare"
colnames(ven_vals_2016)[4] <- "Health"
colnames(ven_vals_2016)[5] <- "Freedom"
colnames(ven_vals_2016)[6] <- "Generosity"
colnames(ven_vals_2016)[7] <- "Corruption"
# 2017
colnames(ven_vals_2017)[1] <- "Score"
colnames(ven_vals_2017)[2] <- "GDP"
colnames(ven_vals_2017)[3] <- "Welfare"
colnames(ven_vals_2017)[4] <- "Health"
colnames(ven_vals_2017)[5] <- "Freedom"
colnames(ven_vals_2017)[6] <- "Generosity"
colnames(ven_vals_2017)[7] <- "Corruption"
# 2018
colnames(ven_vals_2018)[1] <- "Score"
colnames(ven_vals_2018)[2] <- "GDP"
colnames(ven_vals_2018)[3] <- "Welfare"
colnames(ven_vals_2018)[4] <- "Health"
colnames(ven_vals_2018)[5] <- "Freedom"
colnames(ven_vals_2018)[6] <- "Generosity"
colnames(ven_vals_2018)[7] <- "Corruption"
# 2019
colnames(ven_vals_2019)[1] <- "Score"
colnames(ven_vals_2019)[2] <- "GDP"
colnames(ven_vals_2019)[3] <- "Welfare"
colnames(ven_vals_2019)[4] <- "Health"
colnames(ven_vals_2019)[5] <- "Freedom"
colnames(ven_vals_2019)[6] <- "Generosity"
colnames(ven_vals_2019)[7] <- "Corruption"


#Added Year to data
ven_vals_2015 <- mutate(ven_vals_2015, Year = 2015)
ven_vals_2016 <- mutate(ven_vals_2016, Year = 2016)
ven_vals_2017 <- mutate(ven_vals_2017, Year = 2017)
ven_vals_2018 <- mutate(ven_vals_2018, Year = 2018)
ven_vals_2019 <- mutate(ven_vals_2019, Year = 2019)

ven_vals_2015.long <- melt(ven_vals_2015,id.vars ="Year")
ven_vals_2016.long <- melt(ven_vals_2016,id.vars ="Year")
ven_vals_2017.long <- melt(ven_vals_2017,id.vars ="Year")
ven_vals_2018.long <- melt(ven_vals_2018,id.vars ="Year")
ven_vals_2019.long <- melt(ven_vals_2019,id.vars ="Year")


#Recombined Data to allow graphing
ven_vals.long <- rbind(ven_vals_2015.long, ven_vals_2016.long)
ven_vals.long <- rbind(ven_vals.long, ven_vals_2017.long)
ven_vals.long <- rbind(ven_vals.long, ven_vals_2018.long)
ven_vals.long <- rbind(ven_vals.long, ven_vals_2019.long)

#Makes a beautiful graph
ggplot(ven_vals.long, aes(x=variable, y = value, fill=factor(Year)))+
  geom_bar(stat = "identity", position = "dodge") + xlab(NULL) + ylab("Value Score") +
  ggtitle("Changes For Venezuela From 2015-2019") + theme(plot.title = element_text(hjust = 0.5, size = 24), axis.text.y=element_blank(), axis.text.x =element_text(size = 15)) +
  geom_text(aes(label=(sprintf("%0.3f", round(as.double(value), digits = 3)))), position = position_dodge(width=0.9), vjust=1.5) +
  scale_fill_discrete(name = "Year")

