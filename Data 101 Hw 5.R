# Data 101 HW 5
setwd('C:/Users/Owner/Downloads/Data 101 R Assignments')
movies_2022 <- read.csv('Movies2022F-3.csv')
head(movies_2022)
colors <- c('red','blue','cyan','yellow','green','orange')
summary(movies_2022)
# Null: Action movies have about the same average IMDB scores than Comedy movies
# Alt: Action movies have higher average IMDB scores than Comedy movies 
# Hypthosis A
par(mfrow=c(1,2))
Action_movies <- subset(movies_2022, movies_2022$genre == 'Action')
boxplot(Action_movies$imdb_score ~ Action_movies$genre, xlab = 'Action genre',
        ylab = 'IMDB Scores', col = 'blue')
Comedy_movies <- subset(movies_2022, movies_2022$genre == 'Comedy')
boxplot(Comedy_movies$imdb_score ~ Comedy_movies$genre, xlab = 'Comedy genre',
        ylab = 'IMDB Scores', col = 'green')
average_scores_action <- mean(Action_movies$imdb_score)
average_scores_comedy <- mean(Comedy_movies$imdb_score)
sd_scores_action <- sd(Action_movies$imdb_score)
sd_scores_comedy <- sd(Comedy_movies$imdb_score)
len_scores_action <- length(Action_movies$imdb_score)
len_scores_comedy <- length(Comedy_movies$imdb_score)
sd_scores_action_comedy <- sqrt((sd_scores_action^2)/len_scores_action + 
                              (sd_scores_comedy^2)/len_scores_comedy)
zeta <- (average_scores_action - average_scores_comedy)/(sd_scores_action_comedy)
zeta
p = 1 - pnorm(zeta)
p # 0.005132393
#NUll: Drama movies have about the average IMDB scores than  Sci-Fi movies
#Alt: Drama movies have higher average IMDB scores than Sci-Fi movies
par(mfrow=c(1,2))
Subset_Drama <- subset(movies_2022, movies_2022$genre == 'Drama')
boxplot(Subset_Drama$imdb_score ~ Subset_Drama$genre, xlab = 'Drama genre',
        ylab = 'IMDB Scores', col = 'red')
Subset_Sci_Fi <- subset(movies_2022, movies_2022$genre == 'Sci-Fi')
boxplot(Subset_Sci_Fi$imdb_score ~ Subset_Sci_Fi$genre, xlab = 'Sci-Fi genre',
        ylab = 'IMDB Scores', col = 'purple')
average_scores_Drama <- mean(Subset_Drama$imdb_score)
average_scores_Sci_Fi <- mean(Subset_Sci_Fi$imdb_score)
sd_scores_Drama <- sd(Subset_Drama$imdb_score)
sd_scores_Sci_Fi <- sd(Subset_Sci_Fi$imdb_score)
len_scores_Drama <- length(Subset_Drama$imdb_score)
len_scores_Sci_Fi <- length(Subset_Sci_Fi$imdb_score)
sd_scores_Drama_Sci_Fi <- sqrt((sd_scores_Drama^2)/len_scores_Drama + 
                              (sd_scores_Sci_Fi^2)/len_scores_Sci_Fi)
zeta <- (average_scores_Drama - average_scores_Sci_Fi)/(sd_scores_Drama_Sci_Fi)
zeta
p = 1 - pnorm(zeta)
p # 0.4029274
# Null: Medium Budget Comedy movies have the same IMDB scores 
# than Medium Budget Sci-Fi movies.
# Alt: Medium Budget Comedy movies have higher IMDB scores 
# than Medium Budget Sci-Fi movies.
Subset_Budget <- subset(movies_2022, movies_2022$Budget == 'Medium')
par(mfrow=c(1,2))
Subset_Medium_Comedy <- subset(Subset_Budget, Subset_Budget$genre == 'Comedy')
boxplot(Subset_Medium_Comedy$imdb_score ~ Subset_Medium_Comedy$genre, 
        xlab = 'Comedy genre', ylab = 'IMDB Scores', col = 'orange')
Subset_Medium_Sci_Fi <- subset(Subset_Budget, Subset_Budget$genre == 'Sci-Fi')
boxplot(Subset_Medium_Sci_Fi$imdb_score ~ Subset_Medium_Sci_Fi$genre, 
        xlab = 'Sci-Fi genre', ylab = 'IMDB Scores', col = 'yellow')
average_scores_Medium_Comedy <- mean(Subset_Medium_Comedy$imdb_score)
average_scores_Medium_Sci_Fi <- mean(Subset_Medium_Sci_Fi$imdb_score)
sd_scores_Medium_Comedy <- sd(Subset_Medium_Comedy$imdb_score)
sd_scores_Medium_Sci_Fi <- sd(Subset_Medium_Sci_Fi$imdb_score)
len_scores_Medium_Comedy <- length(Subset_Medium_Comedy$imdb_score)
len_scores_Medium_Sci_Fi <- length(Subset_Medium_Sci_Fi$imdb_score)
sd_scores_Comedy_Sci_Fi <- sqrt((sd_scores_Medium_Comedy^2)/len_scores_Medium_Comedy 
                              + (sd_scores_Medium_Sci_Fi^2)/len_scores_Medium_Sci_Fi)
zeta <- (average_scores_Medium_Comedy - average_scores_Medium_Sci_Fi)/(sd_scores_Comedy_Sci_Fi)
zeta
p = 1 - pnorm(zeta)
p # 0.02422866
