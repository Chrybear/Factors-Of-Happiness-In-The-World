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

group_name_score <- twenty_ninteen %>% group_by(Country) %>% arrange(desc(Score)) %>%  select(Score)
top_ten <- head(group_name_score, 10)



top_ten_graph <- ggplot(data = top_ten, aes(x = reorder(Country, -Score), y = Score)) +
       labs(title = "Top 10 Happiest Places in 2019", x = "", y ="Happiness") +
         geom_col()

# Center title text
top_ten_graph <- top_ten_graph + theme(plot.title = element_text(hjust = 0.5))


# Add color
co <- brewer.pal(10, "Set3") 
#top_ten_graph <- top_ten_graph + scale_fill_brewer(type = "seq", palette = "Set3")

# Draw the graph
plot(top_ten_graph)


# Boxplot for bottom 10 2019
bottom_ten <- tail(group_name_score, 10)
bottom_ten <- bottom_ten %>% arrange(Score)

bottom_ten_graph <- ggplot(data = bottom_ten, aes(x = reorder(Country, Score), y = Score)) +
  labs(title = "Bottom 10 Happiest Places in 2019", x = "", y ="Happiness") +
  geom_col()

# Center title text
bottom_ten_graph <- bottom_ten_graph + theme(plot.title = element_text(hjust = 0.5))

# Draw the graph
plot(bottom_ten_graph)


# - Use average to make overall top and bottom 10 for countries from 2015 - 2019

# Top 10

avg_happy <- read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\Averaged_Data.csv')

group_name_score <- avg_happy %>% group_by(Country) %>%  select(Score.Mean)
top_ten_overall <- head(group_name_score, 10)



top_ten_overall_graph <- ggplot(data = top_ten_overall, aes(x = reorder(Country, -Score.Mean), y = Score.Mean)) +
  labs(title = "Top 10 Happiest Places Overall (2015-2019)", x = "", y ="Happiness") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_col()

plot(top_ten_overall_graph)

# Bottom 10

bottom_ten_overall <- tail(group_name_score, 10)



bottom_ten_overall_graph <- ggplot(data = bottom_ten_overall, aes(x = reorder(Country, Score.Mean), y = Score.Mean)) +
  labs(title = "Bottom 10 Unhappiest Places Overall (2015-2019)", x = "", y ="Happiness") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_col()

plot(bottom_ten_overall_graph)








#barplot(top_ten$Score, main = "Top 10 Happiest Places in 2019", ylab = "Score", 
# names.arg = c(top_ten$Country.or.region), 
#ylim = c(0, 10), col = co)

#sad_co <- brewer.pal(9, "Greys") 
#barplot(bottom_ten$Score, main = "Top 10 Unhappiest Places in 2019", 
 #       ylab = "Score", names.arg = c(bottom_ten$Country.or.region), 
  #      ylim = c(0, 10), col = sad_co)