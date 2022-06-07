# Final Exam Prep
setwd('C:/Users/Owner/Downloads/Data 101 R Assignments')
moviess_2022 <- read.csv('Movies2022F-4.csv')
head(moviess_2022)
colors <- c('green','blue','cyan','yellow','red','orange')

# table() displays frequency distribution of its arguments.
table(moviess_2022$country)
table(moviess_2022$content)
table(moviess_2022$imdb_score)
table(moviess_2022$Gross)
table(moviess_2022$Budget)
table(moviess_2022$genre)

# Scatter Plot are used to plot two numerical variables.

# A bar plot are used to plot a categorical variable.
barplot(table(moviess_2022$country), ylab = 'Frequency', col = colors)
barplot(table(moviess_2022$content), ylab = 'Frequency', col = colors)
barplot(table(moviess_2022$imdb_score), ylab = 'Frequency', col = colors)
barplot(table(moviess_2022$Gross), ylab = 'Frequency', col = colors)
barplot(table(moviess_2022$Budget), ylab = 'Frequency', col = colors)
barplot(table(moviess_2022$genre), ylab = 'Frequency', col = colors)

# A boxplot is used to display a numerical variable.
# A boxplot shows the distribution of data in a dataset.

boxplot(imdb_score~country, data = moviess_2022, ylab = 'Score', col = colors)
boxplot(imdb_score~content, data = moviess_2022, ylab = 'Score', col = colors)
boxplot(imdb_score~Gross, data = moviess_2022, ylab = 'Score', col = colors)
boxplot(imdb_score~Budget, data = moviess_2022, ylab = 'Score', col = colors)
boxplot(imdb_score~genre, data = moviess_2022, ylab = 'Score', col = colors)

# Mosaic plot is used to visualize two categorical variables.
mosaicplot(moviess_2022$content~moviess_2022$Gross, col = colors)
mosaicplot(moviess_2022$content~moviess_2022$Budget, col = colors)
mosaicplot(moviess_2022$content~moviess_2022$genre, col = colors)
mosaicplot(moviess_2022$Gross~moviess_2022$Budget, col = colors)
mosaicplot(moviess_2022$Gross~moviess_2022$genre, col = colors)
mosaicplot(moviess_2022$Budget~moviess_2022$genre, col = colors)

#Basic Functions
mean(moviess_2022$imdb_score)
length(moviess_2022$imdb_score)
max(moviess_2022$imdb_score)
min(moviess_2022$imdb_score)
sd(moviess_2022$imdb_score)

# tapply() computes a measure (mean, median, min, max, etc..) 
# or a function for each factor variable in a vector. 
# It is a very useful function that lets you create a subset of a vector 
# and then apply some functions to each of the subset.
# tapply(numerical, categorical, aggregagte function)

tapply(moviess_2022$imdb_score, moviess_2022$country, mean)
tapply(moviess_2022$imdb_score, moviess_2022$content, mean)
tapply(moviess_2022$imdb_score, moviess_2022$Gross, mean)
tapply(moviess_2022$imdb_score, moviess_2022$Budget, mean)
tapply(moviess_2022$imdb_score, moviess_2022$genre, mean)

# Chi-square analysis
data <- table(moviess_2022$content, moviess_2022$genre)
chisq.test(data)
# Chi-Square test is a statistical method to determine 
# if two categorical variables have a significant correlation between them. 

# z value = 2.4, whats the p-value?
1-pnorm(2.4)

# Bayesian Reasoning
# Prior Odds -> Observation -> Posterior Odds
# Odds: p/(1-p)
# Posterior Odds = Likelihood Ratio * Prior Odds
# Prior odds: odds for the belief before observation
# Likelihood Ratio: effect of observation, evidence. Can be larger/smaller than 1.
# Posterior odds: New odds with observation

# Prior odds: P(B)/P(~B)
# Likelihood ratio: P(O|B)/P(O|~B) = P(True Positive)/P(False Positive)
# Posterior odds: P(B|O)/P(~B|O)

# a)
# Belief- high gross
# Observation- low budget movie
head(moviess_2022)

Prior <- nrow(moviess_2022[moviess_2022$Gross == 'High',])/nrow(moviess_2022)
Prior
PriorOdds <- round(Prior/(1-Prior),2)
PriorOdds
# b
pg_rated <- subset(moviess_2022, moviess_2022$content == 'PG')
head(pg_rated)
tapply(pg_rated$imdb_score, pg_rated$genre, mean)
