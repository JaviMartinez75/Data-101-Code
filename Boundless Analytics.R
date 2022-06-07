# Boundless Analytics
setwd('C:/Users/Owner/Downloads/Data 101 R Assignments')
market2022 <- read.csv('HomeworkMarket2022-2.csv')
head(market2022)
dim(market2022)
# Interesting Subset 1
# Location = New Brunswick; Snacks = Crackers; Weekday vs Weekend
# http://209.97.156.178:8082/?dataset=HomeworkMarket2022&first_attr=Day&user_filter=%5B%5B%22Location%22%2C%22categorical%22%2C%5B%22New+Brunswick%22%5D%2Cnull%2Cnull%5D%2C%5B%22Snacks%22%2C%22categorical%22%2C%5B%22Crackers%22%5D%2Cnull%2Cnull%5D%5D

subset1 <- subset(market2022, market2022$Location == 'New Brunswick' & market2022$Snacks == 'Crackers')
head(subset1)
table(subset1$Day) # Weekday - 231/618 = 37.38%, Weekend - 387/618 = 62.62%
table(market2022$Day) # Weekday - 10039/20000 = 50.20% , Weekend - 9961/20000 = 49.80%

market2022$IN <- 'Out_Slice'
market2022[market2022$Location == 'New Brunswick' & market2022$Snacks =='Crackers', ]$IN <- 'In_Slice'
d <- table(market2022$Day, market2022$IN)
chisq.test(d)
# Chi-squared = 41.373 and p-value is 1.258e-10.
# Bonferroni Correction
# Significance level = alpha/N = 0.05/97 = 5.1546e-4
# The significance level is greater than the p-value. Thus, it passes Bonferroni Correction.

# Interesting Subset 2
# SoftDrinks = Cola, Snacks = Popcorn
# http://209.97.156.178:8082/?dataset=HomeworkMarket2022&first_attr=Location&user_filter=%5B%5B%22SoftDrinks%22%2C%22categorical%22%2C%5B%22Cola%22%5D%2Cnull%2Cnull%5D%2C%5B%22Snacks%22%2C%22categorical%22%2C%5B%22Popcorn%22%5D%2Cnull%2Cnull%5D%5D

subset2 <- subset(market2022, market2022$SoftDrinks == 'Cola' & market2022$Snacks == 'Popcorn')
head(subset2)
table(subset2$Location)
# Edison - 159/719 = 22.11%, Metuchen - 128/719 = 17.80%,
# New Brunswick - 144/719 = 20.03%, Princeton - 288/719 = 40.06%
table(market2022$Location)
# Edison - 4958/20000 = 24.79%, Metuchen - 4996/20000 = 24.83%,
# New Brunswick - 4960/20000 = 24.80%, Princeton - 5116/20000 = 25.58%

market2022$IN2 <- 'Out_Slice2'
market2022[market2022$SoftDrinks == 'Cola' & market2022$Snacks == 'Popcorn', ]$IN2 <- 'In_Slice2'
d <- table(market2022$Location, market2022$IN2)
chisq.test(d)
# Chi-squared = 84.932 and p-value is less than 2.2e-16.
# Bonferroni Correction
# Significance level = alpha/N = 0.05/147 = 3.401e-4
# The significance level is greater than the p-value. Thus, it passes Bonferroni Correction.

# Interesting Subset 3
# Day = Weekend, Location = New Brunswick, Snacks = Crackers
# http://209.97.156.178:8082/?dataset=HomeworkMarket2022&first_attr=Beer&user_filter=%5B%5B%22Day%22%2C%22categorical%22%2C%5B%22Weekend%22%5D%2Cnull%2Cnull%5D%2C%5B%22Location%22%2C%22categorical%22%2C%5B%22New+Brunswick%22%5D%2Cnull%2Cnull%5D%2C%5B%22Snacks%22%2C%22categorical%22%2C%5B%22Crackers%22%5D%2Cnull%2Cnull%5D%5D

subset3 <- subset(market2022, market2022$Day == 'Weekend' & market2022$Location == 'New Brunswick' 
                  & market2022$Snacks == 'Crackers')
head(subset3)
table(subset3$Beer) # Ale-66/387 = 17.05%, Lager-228/387 = 58.91%, None-93/387 = 24.03%
table(market2022$Beer) # Ale-6613/20000 = 33.065%, Lager-6613/20000 = 33.065%, None-6774/20000 = 33.87%

market2022$IN3 <- 'Out_Slice3'
market2022[market2022$Day == 'Weekend' & market2022$Location == 'New Brunswick' 
           & market2022$Snacks == 'Crackers', ]$IN3 <- 'In_Slice3'
d <- table(market2022$Beer, market2022$IN3)
chisq.test(d)
# Chi-squared = 121.63 and p-value is less than 2.2e-16.
# Bonferroni Correction
# Significance level = alpha/N = 0.05/108 = 4.63e-4
# The significance level is greater than the p-value. Thus, it passes Bonferroni Correction.


