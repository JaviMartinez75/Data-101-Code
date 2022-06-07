# Data 101 HW 5
setwd('C:/Users/Owner/Downloads/Data 101 R Assignments')
movies_2022 <- read.csv('Movies2022F-3.csv')
head(movies_2022)
colors <- c('red','blue','cyan','yellow','green','orange')

# Null: Medium Budget Comedy movies have the same IMDB scores 
# than Medium Budget Sci-Fi movies.
# Alt: Medium Budget Comedy movies have higher IMDB scores 
# than Medium Budget Sci-Fi movies.
Subset_Budget <- subset(movies_2022, movies_2022$Budget == 'Medium')
par(mfrow=c(1,2))
Subset_Medium_Comedy <- subset(Subset_Budget, Subset_Budget$genre == 'Comedy')
mod1 <- subset(Subset_Medium_Comedy, select = c(3,6))
boxplot(Subset_Medium_Comedy$imdb_score ~ Subset_Medium_Comedy$genre, 
        xlab = 'Comedy genre', ylab = 'IMDB Scores', col = 'orange')

Subset_Medium_Sci_Fi <- subset(Subset_Budget, Subset_Budget$genre == 'Sci-Fi')
mod2 <- subset(Subset_Medium_Sci_Fi, select = c(3,6))
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

mod3 <- rbind(mod1, mod2)

devtools::install_github("devanshagr/PermutationTestSecond")
PermutationTestSecond::Permutation(mod3, "genre", "imdb_score",10000,"Sci-Fi", "Comedy")
