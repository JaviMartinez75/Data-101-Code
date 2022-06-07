# Data 101 HW 11 Prediction Challenge 3
setwd('C:/Users/Owner/Downloads/Data 101 R Assignments')
earnings_training <- read.csv('Earnings_Train2022-1.csv')
head(earnings_training)
earnings_testing <- read.csv('Earnings_Test_Students-1.csv')
head(earnings_testing)
earning_submission <- read.csv('earning_submission.csv')
head(earning_submission)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("ModelMetrics")
library(ModelMetrics)
colors <- c('green','blue','cyan','yellow','red','orange')
install.packages("Ferrari")


########### quick look at the data ##############
barplot(table(earnings_training$GPA))
barplot(table(earnings_training$Number_Of_Professional_Connections))
barplot(table(earnings_training$Major),xlab = 'Major',ylab = 'Frequency' ,col = colors)
barplot(table(earnings_training$Graduation_Year),xlab = 'Graduation Year',ylab = 'Frequency' ,col = colors)
barplot(table(earnings_training$Number_Of_Credits))
barplot(table(earnings_training$Number_Of_Parking_Tickets))
barplot(table(earnings_training$Earnings))

boxplot(earnings_training$Earnings ~ earnings_training$Number_Of_Professional_Connections, 
        xlab = 'Number of Professional Connections', ylab = 'Earnings', col = colors)
boxplot(earnings_training$Earnings ~ earnings_training$Graduation_Year, 
        xlab = 'Graduation Year', ylab = 'Earnings', col = colors)
boxplot(earnings_training$Earnings ~ earnings_training$Number_Of_Credits, 
        xlab = 'Number of Credits', ylab = 'Earnings', col = colors)
boxplot(earnings_training$Earnings ~ earnings_training$Number_Of_Parking_Tickets, 
        xlab = 'Number of Parking Tickets', ylab = 'Earnings', col = colors)

plot(earnings_training$Earnings ~ earnings_training$GPA)
plot(earnings_training$Earnings ~ earnings_training$Number_Of_Professional_Connections)
plot(earnings_training$Earnings ~ earnings_training$Graduation_Year)
plot(earnings_training$Earnings ~ earnings_training$Number_Of_Credits)
plot(earnings_training$Earnings ~ earnings_training$Number_Of_Parking_Tickets)
plot(earnings_training)
############# ############
############ baseline models #############

lm(Earnings ~ GPA + Number_Of_Professional_Connections + Major + Graduation_Year
   + Number_Of_Credits + Number_Of_Parking_Tickets,data = earnings_training)
summary(lm(Earnings ~ GPA + Number_Of_Professional_Connections + Major + Graduation_Year
   + Number_Of_Credits + Number_Of_Parking_Tickets,
   data = earnings_training)) # default lm with all variables

lm(Earnings ~ Number_Of_Professional_Connections + Major,data = earnings_training)
summary(lm(Earnings ~ Number_Of_Professional_Connections + Major,
           data = earnings_training)) # prediction model 1
lm(Earnings ~ GPA + Number_Of_Professional_Connections + Major,data = earnings_training)
summary(lm(Earnings ~ GPA + Number_Of_Professional_Connections + Major,
           data = earnings_training)) # prediction model 2

#########################
new <- (NULL)
earnings_training$new <- new
earnings_training
summary(lm(Earnings ~ Number_Of_Professional_Connections + Major,data = earnings_training))


practice_subset <- subset(earnings_training, earnings_training$Earnings <= 7000)
summary(lm(Earnings ~ GPA + Number_Of_Professional_Connections + Major + Graduation_Year
           + Number_Of_Credits + Number_Of_Parking_Tickets,data = practice_subset))

fit_practice <- rpart(Earnings ~ Number_Of_Professional_Connections,data = practice_subset,
             control = rpart.control(cp = 0.01))
fit_practice
rpart.plot(fit_practice)

fit <- rpart(Earnings ~ Number_Of_Professional_Connections + Major,data = earnings_training)
fit
rpart.plot(fit)

############ Used ML package #########

earnings_nomajor <- subset(earnings_training, select = -c(Major))
round(cor(earnings_nomajor),3)


fit <- rpart(Earnings ~ Number_Of_Professional_Connections + Major,data = earnings_training,
             control = rpart.control(cp = 0.01))
fit
rpart.plot(fit)

######### Prediction and Submission ########
tree <- lm(Earnings ~ Number_Of_Professional_Connections + Major,data = earnings_training)
tree

prediction <- predict(tree, newdata = earnings_testing)
#mean((prediction - earnings_training$Earnings)^2)
mse(earnings_training$Earnings,prediction)
earning_submission$Earnings <- prediction
write.csv(earning_submission, 'earning_submission.csv', row.names=FALSE)

######### Prediction and Submission    ########
####### snippet from online part 1 #####
split <- 0.7*nrow(earnings_training)
split
earnings_trainingTr <- earnings_training[1:split,]
earnings_trainingTr
earnings_trainingTs <- earnings_training[split:nrow(earnings_training),]
train <- lm(Earnings ~ .,  data=earnings_trainingTr)
summary(train)
pred <- predict(train,newdata = earnings_trainingTs)
mean((pred - moodyNUMTs$Earnings)^2)
######## snippet from online part 2/Cross Validation ###########
v<-sample(1:nrow(earnings_training))
v[1:5]
trainScrambled <- earnings_training[v, ]
n <- 100

trainSample<-trainScrambled[nrow(trainScrambled)-n:nrow(trainScrambled), ]
testSample <- trainScrambled[1:n,]

lm.tree <- lm(Earnings ~ Number_Of_Professional_Connections + Major, data=trainSample)
lm.tree
pred2 <- predict(lm.tree,newdata=testSample)
pred2
mse(testSample$Earnings,pred2)

earnings_submission$Grade <- pred2
#write.csv(earnings_submission, 'earnings_submission.csv', row.names=FALSE)
######################




boxplot(earnings_training$Earnings ~ earnings_training$Number_Of_Parking_Tickets, 
        xlab = 'Parking Tickets', ylab = 'Earnings', col = 'orange')
practice1 <- subset(earnings_training, earnings_training$Earnings >= 9000)
boxplot(practice1$Earnings ~ practice1$Number_Of_Parking_Tickets, 
        xlab = 'Parking Tickets', ylab = 'Earnings', col = 'orange')
summary(lm(Earnings ~ Number_Of_Professional_Connections + Major
           + (Number_Of_Parking_Tickets),
           data = practice1))

boxplot(earnings_training$Earnings ~ earnings_training$Number_Of_Credits,
        xlab = 'Number of Credits', ylab = 'Earnings', col = 'orange')
practice2 <- subset(earnings_training, earnings_training$Earnings >= 7000)
boxplot(practice2$Earnings ~ practice2$Number_Of_Credits, 
        xlab = 'Credits', ylab = 'Earnings', col = 'orange')
summary(lm(Earnings ~ Number_Of_Professional_Connections + Major
           + (Number_Of_Credits),
           data = practice2))

boxplot(earnings_training$Earnings ~ earnings_training$GPA,
        xlab = 'GPA', ylab = 'Earnings', col = 'orange')
practice3 <- subset(earnings_training, earnings_training$Earnings >= 7000)
boxplot(practice3$Earnings ~ practice3$GPA, 
        xlab = 'GPA', ylab = 'Earnings', col = 'orange')
summary(lm(Earnings ~ Number_Of_Professional_Connections + Major
           + (GPA),
           data = practice3))


