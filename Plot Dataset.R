# Plot dataset of your choice
getwd()
setwd('C:/Users/Owner/Downloads/Data 101 R Assignments')
insurance <- read.csv('insurance.csv')
head(insurance)

colors <- c('red','blue','cyan','yellow','green','orange')# Assigning different colors

# Plot 1
plot(insurance$age, insurance$charges, xlab = 'Age',ylab = 'Charges',
     main = 'Relationship between Age and Charges')
abline(lm(insurance$charges~insurance$age), col = "blue")
# Plot 2
boxplot(insurance$charges~insurance$smoker, xlab = 'Smoker or Not',
        ylab = 'Charges',main= 'Medical Charges Based on Smoker Status', col = colors)
# Plot 3
barplot(table(insurance$age_range),xlab ='Age Range',ylab = 'Frequency',
        col = colors,main = '# of People Who Have Insurance Based on Age')

boxplot(insurance$charges~insurance$smoker, xlab = 'Smoker or Not',
        ylab = 'Charges',main= 'Medical Charges Based on Smoker Status', col = colors)
boxplot(insurance$charges~insurance$age_range, xlab = 'Age Range',
        ylab = 'Charges',main= 'Medical Charges Based on Age Range', col = colors)
boxplot(insurance$charges~insurance$sex, xlab = 'Gender',
        ylab = 'Charges',main= 'Medical Charges Based on Gender', col = colors)
boxplot(insurance$charges~insurance$children, xlab = 'Children',
        ylab = 'Charges',main= 'Medical Charges Based on Children', col = colors)
boxplot(insurance$charges~insurance$region, xlab = 'Region',
        ylab = 'Charges',main= 'Medical Charges Based on Region', col = colors)
