#Data 101 New HW 4
setwd('C:/Users/Owner/Downloads/Data 101 R Assignments')
movies_2022 <- read.csv('Movies2022F-3.csv')
head(movies_2022)
colors <- c('red','blue','cyan','yellow','green','orange')
# Null: Low Budget PG-13 movies have lower average IMDB scores 
#      than High Budget PG-13 movies
# Alt: Low Budget PG-13 movies have higher average IMDB scores 
#      than High Budget PG-13 movies
par(mfrow=c(1,2))
PG_13_movies <- subset(movies_2022, movies_2022$content == 'PG-13')
PG_13_movies_HighBudget <- subset(PG_13_movies, PG_13_movies$Budget == 'High')
boxplot(PG_13_movies_HighBudget$imdb_score ~ PG_13_movies_HighBudget$Budget,
        xlab = 'High Budget PG-13', ylab = 'IMDB scores', 
        col = 'green')
PG_13_movies_LowBudget <- subset(PG_13_movies, PG_13_movies$Budget == 'Low')
boxplot(PG_13_movies_LowBudget$imdb_score ~ PG_13_movies_LowBudget$Budget,
        xlab = 'Low Budget PG-13', ylab = 'IMDB scores', 
        col = 'red')
average_scores_High_PG13 <- mean(PG_13_movies_HighBudget$imdb_score)
average_scores_Low_PG13 <- mean(PG_13_movies_LowBudget$imdb_score)
#NUll: History movies have lower average IMDB scores than Family movies
#Alt: History movies have higher average IMDB scores than Family movies
par(mfrow=c(1,2))
History_movies <- subset(movies_2022, movies_2022$genre == 'History')
boxplot(History_movies$imdb_score ~ History_movies$genre, xlab = 'History genre',
        ylab = 'IMDB Score', col = 'purple')
Family_movies <- subset(movies_2022, movies_2022$genre == 'Family')
boxplot(Family_movies$imdb_score ~ Family_movies$genre, xlab = 'Family genre',
        ylab = 'IMDB score', col = 'red')
average_scores_history <- mean(History_movies$imdb_score)
average_scores_family <- mean(Family_movies$imdb_score)

# Null: USA Movies have lower average IMDB scores than UK Movies.
# Alt: USA Movies have higher average IMDB scores than UK Movies.
par(mfrow=c(1,2))
USA_Movies <- subset(movies_2022, movies_2022$country == 'USA')
boxplot(USA_Movies$imdb_score ~ USA_Movies$country, xlab = 'USA Movies',
        ylab = 'IMDB Scores', col = 'blue')
NZ_Movies <- subset(movies_2022, movies_2022$country == 'New Zealand')
boxplot(NZ_Movies$imdb_score ~ NZ_Movies$country, xlab = 'New Zealand Movies',
        ylab = 'IMDB Scores', col = 'green')
average_scores_USA <- mean(USA_Movies$imdb_score)
average_scores_NZ <- mean(NZ_Movies$imdb_score)