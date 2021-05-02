
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
#   xlab("Country") + ylab("Yearly Happiness Rank") + ggtitle("Rank Changes Among Top 10 Happiest") +
#   theme(plot.title = element_text(hjust = 0.5))

#Grouped by Country (probably best, no need for by year?)
ggplot(top_happy_ranks.long, aes(x=Country,y=value, fill=factor(variable)))+
  geom_bar(stat = "identity", position = "dodge")+
  xlab("Country") + ylab("Happiness Rank (Lower = Higher Rank)") + ggtitle("Rank Changes Among Top 10 Happiest") +
  theme(plot.title = element_text(hjust = 0.5, size = 24), axis.text.x = element_text(size = 20)) + 
  geom_text(aes(label=value), position = position_dodge(width=0.9), vjust=1.5, size = 5) +
  scale_y_continuous(expand=c(0,0), limits = c(0, 11.5)) + scale_fill_discrete(name = "Rank That Year")


#Bottom 10 Rank Graph
#Grouped by year
# ggplot(bottom_happy_ranks.long, aes(x=variable,y=value, fill=factor(Country)))+
#   geom_bar(stat = "identity", position = "dodge")+
#   xlab("Country") + ylab("Yearly Happiness Rank")+
#   coord_cartesian(ylim = c(140, NA)) + ggtitle("Rank Changes Among Bottom 10 Happiest") +
#   theme(plot.title = element_text(hjust = 0.5))

#Grouped by Country (probably best, no need for by year?)
ggplot(bottom_happy_ranks.long, aes(x=Country,y=value, fill=factor(variable)))+
  geom_bar(stat = "identity", position = "dodge")+
  xlab(NULL) + ylab("Happiness Rank (Lower = Higher Rank)")+
  coord_cartesian(ylim = c(140, NA)) + ggtitle("Rank Changes Among Bottom 10 Happiest") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 20)) + 
  geom_text(aes(label=value), position = position_dodge(width=0.9), vjust=1.5) +
  scale_fill_discrete(name = "Rank That Year")






#GDP Scatter PLot
GDP.Cor <- cor.test(avg_happy$GDP.Mean, avg_happy$Score.Mean, method = "pearson" )
ggplot(avg_happy, aes(x= GDP.Mean, y = Score.Mean)) +
  geom_point()+
  geom_smooth(method = lm, color = "red", fill="grey", fullrange = TRUE,  se = TRUE)+
  geom_label(label = paste("Sample Estimate: ", format((GDP.Cor$estimate),digits = 4)), x = 0.375, y=6.5, color = "red")+
  ggtitle("GDP vs Happiness Correlation") + xlab("Mean GDP Score") + ylab("Mean Happiness Score")+
  theme_ipsum()

#Social Scatter PLot
Social.Cor <- cor.test(avg_happy$Social.support.Mean, avg_happy$Score.Mean, method = "pearson" )
ggplot(avg_happy, aes(x= Social.support.Mean, y = Score.Mean)) +
  geom_point()+
  geom_smooth(method = lm, color = "red", fill="grey", fullrange = TRUE,  se = TRUE)+
  geom_label(label = paste("Sample Estimate: ", format((Social.Cor$estimate),digits = 4)), x = 0.375, y=6.5, color = "red")+
  ggtitle("Social vs Happiness Correlation") + xlab("Mean Social Score") + ylab("Mean Happiness Score")+
  theme_ipsum()

#Health Scatter Plot
Health.Cor <- cor.test(avg_happy$Health.Mean, avg_happy$Score.Mean, method = "pearson" )

ggplot(avg_happy, aes(x= Health.Mean, y = Score.Mean)) +
  geom_point()+
  geom_smooth(method = lm, color = "red", fill="grey", fullrange = TRUE,  se = TRUE)+
  geom_label(label = paste("Sample Estimate: ", format((Health.Cor$estimate),digits = 4)), x = 0.375, y=6.5, color = "red")+
  ggtitle("Health vs Happiness Correlation") + xlab("Mean Health Score") + ylab("Mean Happiness Score")+
  theme_ipsum()

#Freedom Scatter Plot
Freedom.Cor <- cor.test(avg_happy$Freedom.Mean, avg_happy$Score.Mean, method = "pearson" )

ggplot(avg_happy, aes(x= Freedom.Mean, y = Score.Mean)) +
  geom_point()+
  geom_smooth(method = lm, color = "red", fill="grey", fullrange = TRUE,  se = TRUE)+
  geom_label(label = paste("Sample Estimate: ", format((Freedom.Cor$estimate),digits = 4)), x = 0.375, y=6.5, color = "red")+
  ggtitle("Freedom vs Happiness Correlation") + xlab("Mean Freedom Score") + ylab("Mean Happiness Score")+
  theme_ipsum()

#Generosity Scatter Plot
Generosity.Cor <- cor.test(avg_happy$Generosity.Mean, avg_happy$Score.Mean, method = "pearson" )

ggplot(avg_happy, aes(x= Generosity.Mean, y = Score.Mean)) +
  geom_point()+
  geom_smooth(method = lm, color = "red", fill="grey", fullrange = TRUE,  se = TRUE)+
  geom_label(label = paste("Sample Estimate: ", format((Generosity.Cor$estimate),digits = 4)), x = 0.375, y=6.5, color = "red")+
  ggtitle("Generosity vs Happiness Correlation") + xlab("Mean Generosity Score") + ylab("Mean Happiness Score")+
  theme_ipsum()

#Perception Scatter Plot
Perceptions.Cor <- cor.test(avg_happy$Perceptions.of.corruption.Mean, avg_happy$Score.Mean, method = "pearson" )

ggplot(avg_happy, aes(x= Perceptions.of.corruption.Mean, y = Score.Mean)) +
  geom_point()+
  geom_smooth(method = lm, color = "red", fill="grey", fullrange = TRUE,  se = TRUE)+
  geom_label(label = paste("Sample Estimate: ", format((Perceptions.Cor$estimate),digits = 4)), x = 0.375, y=6.5, color = "red")+
  ggtitle("Perception of Corruption vs Happiness Correlation") + xlab("Mean Perception Score") + ylab("Mean Happiness Score")+
  theme_ipsum()


