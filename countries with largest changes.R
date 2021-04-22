# Find which country had the biggest overall improvement in happiness from 2015 to 2019

avg_happy <- read.csv(file = 'Z:\\Documents\\Spring 2021\\CSC 583\\Team Project\\Factors-Of-Happiness-In-The-World\\happy_data\\Averaged_Data.csv')

big_happy <- avg_happy %>% arrange(desc(Score.2019 - Score.2015))

big_sad <- avg_happy %>% arrange(Score.2019 - Score.2015)

head(big_happy,1)
head(big_sad,1)

# Biggest increase: Benin
# Biggest decrease: Venezuela