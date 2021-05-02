# Authors: Charles Ryan Barrett 
# Date:
# Description: R script file for top and bottom 10 happiest places

library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Top 10 happiest places as of most recent data (2019)

# v needs to be altered per computer. Can be automated?

twenty_ninteen <- read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\2019.csv')

# Bar graph for happiest in 2019


group_name_score <- twenty_ninteen %>% group_by(Country) %>% arrange(desc(Score)) %>%  select(Score)
top_ten <- head(group_name_score, 10)

# Graph it
ggplot(top_ten, aes(x=reorder(Country, -Score), y = Score, fill=factor(reorder(Country, -Score))))+
  geom_bar(stat = "identity", position = "dodge") + xlab(NULL) + ylab("Happiness") +
  ggtitle("Top 10 Happiest Places in 2019") + theme(plot.title = element_text(hjust = 0.5, size = 24), axis.text.x = element_text(size = 20)) +
  geom_text(aes(label=Score), position = position_dodge(width=0.9), vjust=1.5, size = 6) + scale_fill_discrete(name = "Country") +
  scale_y_continuous(expand=c(0,0), limits = c(0, max(top_ten$Score) + 0.5))


# Bar graph for bottom 10 2019
bottom_ten <- tail(group_name_score, 10)
bottom_ten <- bottom_ten %>% arrange(Score)

# Graph it
ggplot(bottom_ten, aes(x=reorder(Country, Score), y = Score, fill=factor(reorder(Country, Score))))+
  geom_bar(stat = "identity", position = "dodge") + xlab(NULL) + ylab("Happiness") +
  ggtitle("Bottom 10 Happiest Places in 2019") + theme(plot.title = element_text(hjust = 0.5, size = 24), axis.text.x = element_text(size = 15)) +
  geom_text(aes(label=Score), position = position_dodge(width=0.9), vjust=1.5, size = 6) + scale_fill_discrete(name = "Country") +
  scale_y_continuous(expand=c(0,0), limits = c(0, max(bottom_ten$Score.Mean) + 0.5))


# - Use average to make overall top and bottom 10 for countries from 2015 - 2019

# Top 10

avg_happy <- read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\Averaged_Data.csv')

group_name_score <- avg_happy %>% group_by(Country) %>%  select(Score.Mean)
top_ten_overall <- head(group_name_score, 10)

# Graph it
ggplot(top_ten_overall, aes(x=reorder(Country, -Score.Mean), y = Score.Mean, fill=factor(reorder(Country, -Score.Mean))))+
  geom_bar(stat = "identity", position = "dodge") + xlab(NULL) + ylab("Happiness") +
  ggtitle("Top 10 Happiest Places Overall") + theme(plot.title = element_text(hjust = 0.5, size = 24), axis.text.x = element_text(size = 20)) +
  geom_text(aes(label=Score.Mean), position = position_dodge(width=0.9), vjust=1.5, size = 6) + scale_fill_discrete(name = "Country") +
  scale_y_continuous(expand=c(0,0), limits = c(0, max(top_ten_overall$Score.Mean) + 0.5))

# Bottom 10

bottom_ten_overall <- tail(group_name_score, 10)

# Graph it
ggplot(bottom_ten_overall, aes(x=reorder(Country, Score.Mean), y = Score.Mean, fill=factor(reorder(Country, Score.Mean))))+
  geom_bar(stat = "identity", position = "dodge") + xlab(NULL) + ylab("Happiness") +
  ggtitle("Bottom 10 Happiest Places Overall") + theme(plot.title = element_text(hjust = 0.5, size = 24), axis.text.x = element_text(size = 15)) +
  geom_text(aes(label=Score.Mean), position = position_dodge(width=0.9), vjust=1.5, size = 6) + scale_fill_discrete(name = "Country") +
  scale_y_continuous(expand=c(0,0), limits = c(0, max(bottom_ten_overall$Score.Mean) + 0.5))








#barplot(top_ten$Score, main = "Top 10 Happiest Places in 2019", ylab = "Score", 
# names.arg = c(top_ten$Country.or.region), 
#ylim = c(0, 10), col = co)

#sad_co <- brewer.pal(9, "Greys") 
#barplot(bottom_ten$Score, main = "Top 10 Unhappiest Places in 2019", 
 #       ylab = "Score", names.arg = c(bottom_ten$Country.or.region), 
  #      ylim = c(0, 10), col = sad_co)