# Authors: Charles Ryan Barrett 
# Date:
# Description: R script file for top and bottom 10 happiest places

# Top 10 happiest places as of most recent data (2019)

# v needs to be altered per computer. Can be automated?

twenty_ninteen <- read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\2019.csv')

# Boxplot for top 10 happiest 2019
library(dplyr)
library(ggplot2)
library(RColorBrewer)

group_name_score <- twenty_ninteen %>% group_by(Country.or.region) %>% arrange(desc(Score)) %>%  select(Score)
top_ten <- head(group_name_score, 10)

co <- brewer.pal(10, "Set3") 
barplot(top_ten$Score, main = "Top 10 Happiest Places in 2019", ylab = "Score", 
        names.arg = c(top_ten$Country.or.region), 
        ylim = c(0, 10), col = co)


# Boxplot for bottom 10 2019
bottom_ten <- tail(group_name_score, 10)
bottom_ten <- bottom_ten %>% arrange(Score)
sad_co <- brewer.pal(9, "Greys") 
barplot(bottom_ten$Score, main = "Top 10 Unhappiest Places in 2019", 
        ylab = "Score", names.arg = c(bottom_ten$Country.or.region), 
        ylim = c(0, 10), col = sad_co)



