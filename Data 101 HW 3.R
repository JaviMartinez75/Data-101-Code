# Data 101 HW 3
setwd('C:/Users/Owner/Downloads/Data 101 R Assignments')
moody2022_new <- read.csv('moody2022_new.csv')
head(moody2022_new)
colors <- c('red','blue','cyan','yellow','green','orange')

boxplot(moody2022_new$SCORE ~ moody2022_new$DOZES_OFF, xlab = 'Dozing Off',
        ylab = 'Score', main= 'Boxplot of Scores Based on Dozing Off', 
        col = colors)

boxplot(moody2022_new$SCORE ~ moody2022_new$TEXTING_IN_CLASS, 
        xlab = 'Texting Frequency', ylab = 'Score', 
        main= 'Boxplot of Scores Based on Texting In Class', col = colors)

plot(moody2022_new$PARTICIPATION, moody2022_new$SCORE, xlab = 'Participation', 
     ylab = 'Scores', main = 'Relationship Between Participation and Score', 
     col = 'red') # Plot 1
lm(moody2022_new$SCORE ~ moody2022_new$PARTICIPATION)
abline(lm(moody2022_new$SCORE ~ moody2022_new$PARTICIPATION), col = "blue")


mean(moody2022_new[moody2022_new$SCORE > 80,]$PARTICIPATION)

min(movies[movies$Budget=='High',]$imdb)

max(mean(moody2022_new[moody2022_new$DOZES_OFF == 'always',]$PARTICIPATION))


table(movies[movies$country=='UK',]$genre, movies[movies$country=='UK',]$country)
table(moody2022_new$TEXTING_IN_CLASS)
