# HW 7 Data Driven Blog
setwd('C:/Users/Owner/Downloads/Data 101 R Assignments')
insurance <- read.csv('insurance.csv')
head(insurance)
colors <- c('blue','red','cyan','yellow','green','orange')

# Null: Smokers have lower medical charges than non-smokers.
# Alt: Smokers have higher medical charges than non-smokers.
boxplot(insurance$charges~insurance$smoker, xlab = 'Smoker or Not',
        ylab = 'Charges',main= 'Medical Charges Based on Smoker Status', col = colors)
barplot(table(insurance$smoker))

par(mfrow=c(1,2))
Subset_Smoker <- subset(insurance, insurance$smoker == 'yes')
#mod1 <- subset(Subset_Medium_Comedy, select = c(3,6))
boxplot(Subset_Smoker$charges ~ Subset_Smoker$smoker, 
        xlab = 'Smoker', ylab = 'Charges', col = 'orange')

Subset_Non_Smoker <- subset(insurance, insurance$smoker == 'no')
#mod2 <- subset(Subset_Medium_Sci_Fi, select = c(3,6))
boxplot(Subset_Smoker$charges ~ Subset_Smoker$smoker, 
        xlab = 'Non-Smoker', ylab = 'Charges', col = 'yellow')

average_scores_Subset_Smoker <- mean(Subset_Smoker$charges)
average_scores_Subset_Non_Smoker <- mean(Subset_Non_Smoker$charges)
sd_scores_Subset_Smoker <- sd(Subset_Smoker$charges)
sd_scores_Subset_Non_Smoker <- sd(Subset_Non_Smoker$charges)
len_scores_Subset_Smoker <- length(Subset_Smoker$charges)
len_scores_Subset_Non_Smoker <- length(Subset_Non_Smoker$charges)

sd_scores_Smoker_Statues <- sqrt((sd_scores_Subset_Smoker^2)/len_scores_Subset_Smoker 
                                + (sd_scores_Subset_Non_Smoker^2)/len_scores_Subset_Non_Smoker)
zeta <- (average_scores_Subset_Smoker - average_scores_Subset_Non_Smoker)/(sd_scores_Smoker_Statues)
zeta
p = 1 - pnorm(zeta)
p # p is 0.

# Null: Males have lower medical charges than females.
# Alt: Males have higher medical charges than females.
boxplot(insurance$charges~insurance$sex, xlab = 'Gender',
        ylab = 'Charges',main= 'Medical Charges Based on Gender', col = colors)
par(mfrow=c(1,2))
Subset_Male_Sex <- subset(insurance, insurance$sex == 'male')
#mod1 <- subset(Subset_Medium_Comedy, select = c(3,6))
boxplot(Subset_Male_Sex$charges ~ Subset_Male_Sex$sex, 
        xlab = 'Male', ylab = 'Charges', col = 'orange')

Subset_Female_Sex <- subset(insurance, insurance$sex == 'female')
#mod2 <- subset(Subset_Medium_Sci_Fi, select = c(3,6))
boxplot(Subset_Female_Sex$charges ~ Subset_Female_Sex$sex, 
        xlab = 'Female', ylab = 'Charges', col = 'yellow')

average_scores_Male <- mean(Subset_Male_Sex$charges)
average_scores_Female <- mean(Subset_Female_Sex$charges)
sd_scores_Subset_Male <- sd(Subset_Male_Sex$charges)
sd_scores_Subset_Female <- sd(Subset_Female_Sex$charges)
len_scores_Subset_Male <- length(Subset_Male_Sex$charges)
len_scores_Subset_Female <- length(Subset_Female_Sex$charges)

sd_scores_Gender_Status <- sqrt((sd_scores_Subset_Male^2)/len_scores_Subset_Male
                                 + (sd_scores_Subset_Female^2)/len_scores_Subset_Female)
zeta <- (average_scores_Male - average_scores_Female)/(sd_scores_Gender_Status)
zeta
p = 1 - pnorm(zeta)
p # p is 0.01782541.
