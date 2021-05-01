# Check correlation between attributes and happiness/sadness/other factors

library(dplyr)
library(ggplot2)


# Compare correlation between health and welfare for the Top 10 overall happiest

avg_happy <- read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\Averaged_Data.csv')


# Get top 10 happiest overall
top_ten_overall <- head(avg_happy %>% arrange(desc(Score.Mean)), 10)


ggplot(data = top_ten_overall, aes(x = Social.support.Mean, y = Health.Mean)) +
  labs(title = "Correlation of Welfare and Health Among top 10 Happiest", x = "Welfare", y ="Health") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_line()

# Get bottom 10 happiest overall

bottom_ten_overall <- tail(avg_happy, 10)

ggplot(data = bottom_ten_overall, aes(x = Social.support.Mean, y = Health.Mean)) +
  labs(title = "Correlation of Welfare and Health Among bottom 10 Happiest", x = "Welfare", y ="Health") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_line()

# Among entire data
ggplot(data = avg_happy, aes(x = Social.support.Mean, y = Health.Mean)) +
  labs(title = "Correlation of Welfare and Health Among All", x = "Welfare", y ="Health") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_line()
