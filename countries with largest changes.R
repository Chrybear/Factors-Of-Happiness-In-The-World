library(ggplot2)
library(dplyr)

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

# Get each year's values
benin_vals_2015 <- big_happy %>% select(rank.2015, Score.2015, GDP.2015, Social.2015, Health.2015, Freedom.2015, Generosity.2015, Perceptions.of.corruption.2015)
benin_vals_2016 <- big_happy %>% select(rank.2016, Score.2016, GDP.2016, Social.2016, Health.2016, Freedom.2016, Generosity.2016, Perceptions.of.corruption.2016)
benin_vals_2017 <- big_happy %>% select(rank.2017, Score.2017, GDP.2017, Social.2017, Health.2017, Freedom.2017, Generosity.2017, Perceptions.of.corruption.2017)
benin_vals_2018 <- big_happy %>% select(rank.2018, Score.2018, GDP.2018, Social.2018, Health.2018, Freedom.2018, Generosity.2018, Perceptions.of.corruption.2018)
benin_vals_2019 <- big_happy %>% select(rank.2019, Score.2019, GDP.2019, Social.2019, Health.2019, Freedom.2019, Generosity.2019, Perceptions.of.corruption.2019)

class(benin_vals_2015)
ggplot(benin_vals_2015, aes(x = c()))





ven_vals_2015 <- big_sad %>% select(rank.2015, Score.2015, GDP.2015, Social.2015, Health.2015, Freedom.2015, Generosity.2015, Perceptions.of.corruption.2015)
ven_vals_2016 <- big_sad %>% select(rank.2016, Score.2016, GDP.2016, Social.2016, Health.2016, Freedom.2016, Generosity.2016, Perceptions.of.corruption.2016)
ven_vals_2017 <- big_sad %>% select(rank.2017, Score.2017, GDP.2017, Social.2017, Health.2017, Freedom.2017, Generosity.2017, Perceptions.of.corruption.2017)
ven_vals_2018 <- big_sad %>% select(rank.2018, Score.2018, GDP.2018, Social.2018, Health.2018, Freedom.2018, Generosity.2018, Perceptions.of.corruption.2018)
ven_vals_2019 <- big_sad %>% select(rank.2019, Score.2019, GDP.2019, Social.2019, Health.2019, Freedom.2019, Generosity.2019, Perceptions.of.corruption.2019)


top_happy_ranks.long <- melt(top_happy_ranks,id.vars ="Country")

table(big_happy)
table(big_sad)






table(big_happy)
