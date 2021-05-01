
install.packages("hrbrthemes")

library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(hrbrthemes)

# Get the averaged data (need to change per PC)
avg_happy <- read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\Averaged_Data.csv')

################################
## Reformatting for graphing ##
##############################
avg_happy %>% arrange(desc(Score.Mean))

#Top ranks
top_happy_ranks <- avg_happy %>% select(Country,rank.2015,rank.2016,rank.2017,rank.2018,rank.2019, rank.mean)
top_happy_ranks <- top_happy_ranks %>% arrange(rank.mean) %>% head(10)

top_happy_ranks.long <- melt(top_happy_ranks,id.vars ="Country")

#Bottom ranks
bottom_happy_ranks <- avg_happy %>% select(Country,rank.2015,rank.2016,rank.2017,rank.2018,rank.2019, rank.mean)
bottom_happy_ranks <- bottom_happy_ranks %>% arrange(desc(rank.mean)) %>% head(10)

bottom_happy_ranks.long <- melt(bottom_happy_ranks,id.vars ="Country")



####################
## Graphing data ##
##################

#Top 10 Rank Graph
#Grouped by year
# ggplot(top_happy_ranks.long, aes(x=variable,y=value, fill=factor(Country)))+
#   geom_bar(stat = "identity", position = "dodge")+
#   xlab("Country") + ylab("Yearly Happiness Rank") + ggtitle("Rank Changes Amongst Top 10 Happiest") +
#   theme(plot.title = element_text(hjust = 0.5))

#Grouped by Country (probably best, no need for by year?)
ggplot(top_happy_ranks.long, aes(x=Country,y=value, fill=factor(variable)))+
  geom_bar(stat = "identity", position = "dodge")+
  xlab("Country") + ylab("Yearly Happiness Rank (Lower = Higher Rank)") + ggtitle("Rank Changes Amongst Top 10 Happiest") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=top_happy_ranks.long$value), position = position_dodge(width=0.9), vjust=1.5)


#Bottom 10 Rank Graph
#Grouped by year
# ggplot(bottom_happy_ranks.long, aes(x=variable,y=value, fill=factor(Country)))+
#   geom_bar(stat = "identity", position = "dodge")+
#   xlab("Country") + ylab("Yearly Happiness Rank")+
#   coord_cartesian(ylim = c(140, NA)) + ggtitle("Rank Changes Amongst Bottom 10 Happiest") +
#   theme(plot.title = element_text(hjust = 0.5))

#Grouped by Country (probably best, no need for by year?)
ggplot(bottom_happy_ranks.long, aes(x=Country,y=value, fill=factor(variable)))+
  geom_bar(stat = "identity", position = "dodge")+
  xlab("Country") + ylab("Yearly Happiness Rank (Lower = Higher Rank)")+
  coord_cartesian(ylim = c(140, NA)) + ggtitle("Rank Changes Amongst Top 10 Happiest") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=bottom_happy_ranks.long$value), position = position_dodge(width=0.9), vjust=1.5)






#GDP Scatter PLot
GDP.Cor <- cor.test(total_means$GDP.Mean, total_means$Score.Mean, method = "pearson" )
ggplot(total_means, aes(x= GDP.Mean, y = Score.Mean)) +
  geom_point()+
  geom_smooth(method = lm, color = "red", fill="grey", fullrange = TRUE,  se = TRUE)+
  geom_label(label = paste("Sample Estimate: ", format((GDP.Cor$estimate),digits = 4)), x = 0.375, y=6.5, color = "red")+
  theme_ipsum()

#Social Scatter PLot
Social.Cor <- cor.test(total_means$Social.support.Mean, total_means$Score.Mean, method = "pearson" )
ggplot(total_means, aes(x= Social.support.Mean, y = Score.Mean)) +
  geom_point()+
  geom_smooth(method = lm, color = "red", fill="grey", fullrange = TRUE,  se = TRUE)+
  geom_label(label = paste("Sample Estimate: ", format((Social.Cor$estimate),digits = 4)), x = 0.375, y=6.5, color = "red")+
  theme_ipsum()

#Health Scatter Plot
Health.Cor <- cor.test(total_means$Health.Mean, total_means$Score.Mean, method = "pearson" )

ggplot(total_means, aes(x= Health.Mean, y = Score.Mean)) +
  geom_point()+
  geom_smooth(method = lm, color = "red", fill="grey", fullrange = TRUE,  se = TRUE)+
  geom_label(label = paste("Sample Estimate: ", format((Health.Cor$estimate),digits = 4)), x = 0.375, y=6.5, color = "red")+
  theme_ipsum()

#Freedom Scatter Plot
Freedom.Cor <- cor.test(total_means$Freedom.Mean, total_means$Score.Mean, method = "pearson" )

ggplot(total_means, aes(x= Freedom.Mean, y = Score.Mean)) +
  geom_point()+
  geom_smooth(method = lm, color = "red", fill="grey", fullrange = TRUE,  se = TRUE)+
  geom_label(label = paste("Sample Estimate: ", format((Freedom.Cor$estimate),digits = 4)), x = 0.375, y=6.5, color = "red")+
  theme_ipsum()

#Generosity Scatter Plot
Generosity.Cor <- cor.test(total_means$Generosity.Mean, total_means$Score.Mean, method = "pearson" )

ggplot(total_means, aes(x= Generosity.Mean, y = Score.Mean)) +
  geom_point()+
  geom_smooth(method = lm, color = "red", fill="grey", fullrange = TRUE,  se = TRUE)+
  geom_label(label = paste("Sample Estimate: ", format((Generosity.Cor$estimate),digits = 4)), x = 0.375, y=6.5, color = "red")+
  theme_ipsum()

#Perception Scatter Plot
Perceptions.Cor <- cor.test(total_means$Perceptions.of.corruption.Mean, total_means$Score.Mean, method = "pearson" )

ggplot(total_means, aes(x= Perceptions.of.corruption.Mean, y = Score.Mean)) +
  geom_point()+
  geom_smooth(method = lm, color = "red", fill="grey", fullrange = TRUE,  se = TRUE)+
  geom_label(label = paste("Sample Estimate: ", format((Perceptions.Cor$estimate),digits = 4)), x = 0.375, y=6.5, color = "red")+
  theme_ipsum()


