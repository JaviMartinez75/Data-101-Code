# Data 101 HW 10 Prediction 2
setwd('C:/Users/Owner/Downloads/Data 101 R Assignments')
originial_moody <- read.csv('M2022train.csv')
tail(originial_moody)
missing_moody <- read.csv('M2022testSNoGrade.csv')
tail(missing_moody)
submission2 <- read.csv('M2022submission.csv')
head(submission2)
colors <- c('green','blue','cyan','yellow','red','orange')
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

tree <- rpart(Grade ~ Major + Score + Seniority, data = originial_moody, method = "class")
tree
rpart.plot(tree)

tree2 <- rpart(Grade ~ Major + Score + Seniority, data = originial_moody, method = "class", 
               control = rpart.control(minsplit = 10))
tree2
rpart.plot(tree2)

tree3 <- rpart(Grade ~ Major + Score + Seniority, data = originial_moody, method = "class", 
               control = rpart.control(minbucket = 10))
tree3
rpart.plot(tree3)

tree4 <- rpart(Grade ~ ., data = originial_moody,method = "class", 
               control = rpart.control(cp = 0.0001))
tree4
rpart.plot(tree4)

#########   1st attempt   #################
table(originial_moody[originial_moody$Score >= 66.5 
                      & originial_moody$Score <= 100,]$Grade)#A-196,B-67,C-25,D-1,F-1
table(originial_moody[originial_moody$Score >= 87.5 
                      & originial_moody$Score <= 100,]$Grade)#A-143,B-7,C-1,D-1,F-1 (Pick this one)
table(originial_moody[originial_moody$Score >= 66.5 
                      & originial_moody$Score < 87.5,]$Grade)#A-53,B-60,C-24,D-0,F-0
table(originial_moody[originial_moody$Score >= 66.5 & originial_moody$Score < 87.5 
                      & (originial_moody$Major == 'Economics' 
                         | originial_moody$Major == 'Psychology'),]$Grade)#A-40,B-15,C-1,D-0,F-0  (Pick this one)
table(originial_moody[originial_moody$Score >= 66.5 & originial_moody$Score < 87.5 
                      & (originial_moody$Major == 'CS' 
                         | originial_moody$Major == 'Statistics'),]$Grade)#A-13,B-45,C-23,D-0,F-0
table(originial_moody[originial_moody$Score >= 66.5 & originial_moody$Score < 87.5 
                      & (originial_moody$Major == 'CS' 
                         | originial_moody$Major == 'Statistics') 
                      & originial_moody$Seniority == 'Freshman',]$Grade)#A-13,B-13
table(originial_moody[originial_moody$Score >= 77 & originial_moody$Score < 87.5 
                      & (originial_moody$Major == 'CS' 
                         | originial_moody$Major == 'Statistics') 
                      & originial_moody$Seniority == 'Freshman',]$Grade)#A-13,B-3 (Pick this one)
table(originial_moody[originial_moody$Score >= 66.5 & originial_moody$Score < 77 
                      & (originial_moody$Major == 'CS' 
                         | originial_moody$Major == 'Statistics') 
                      & originial_moody$Seniority == 'Freshman',]$Grade)#A-0,B-10 (Pick this one)
table(originial_moody[originial_moody$Score >= 66.5 & originial_moody$Score < 87.5 
                      & (originial_moody$Major == 'CS' 
                         | originial_moody$Major == 'Statistics') 
                      & originial_moody$Seniority != 'Freshman',]$Grade)#B-32,C-23
table(originial_moody[originial_moody$Score >= 80.5 & originial_moody$Score < 87.5 
                      & (originial_moody$Major == 'CS' 
                         | originial_moody$Major == 'Statistics') 
                      & originial_moody$Seniority != 'Freshman',]$Grade)#B-27,C-1 (Pick this one)
table(originial_moody[originial_moody$Score >= 66.5 & originial_moody$Score < 80.5 
                      & (originial_moody$Major == 'CS' 
                         | originial_moody$Major == 'Statistics') 
                      & originial_moody$Seniority != 'Freshman',]$Grade)#B-5,C-22 (Pick this one)

table(originial_moody[originial_moody$Score >= 0 
                      & originial_moody$Score < 66.5,]$Grade)#A-8,B-29,C-84,D-95,F-194
table(originial_moody[originial_moody$Score >= 46.5 
                      & originial_moody$Score < 66.5,]$Grade)#A-5,B-27,C-65,D-24,F-6
table(originial_moody[originial_moody$Score >= 46.5 
                      & originial_moody$Score < 66.5
                      & originial_moody$Seniority == 'Freshman',]$Grade)#A-5,B-16,C-14,D-1,F-0
table(originial_moody[originial_moody$Score >= 58.5 
                      & originial_moody$Score < 66.5
                      & originial_moody$Seniority == 'Freshman',]$Grade)#A-5,B-3,C-0,D-0,F-0  (Pick this one)
table(originial_moody[originial_moody$Score >= 46.5 
                      & originial_moody$Score < 58.5
                      & originial_moody$Seniority == 'Freshman',]$Grade)#A-0,B-13,C-14,D-1,F-0
table(originial_moody[originial_moody$Score >= 46.5 
                      & originial_moody$Score < 58.5
                      & originial_moody$Seniority == 'Freshman'
                      & (originial_moody$Major == 'Economics' 
                       | originial_moody$Major == 'Psychology'),]$Grade)#A-0,B-10,C-3,D-0,F-0 (Pick this one)
table(originial_moody[originial_moody$Score >= 46.5 
                      & originial_moody$Score < 58.5
                      & originial_moody$Seniority == 'Freshman'
                      & (originial_moody$Major == 'CS' 
                         | originial_moody$Major == 'Statistics'),]$Grade)#A-0,B-3,C-11,D-1,F-0 (Pick this one)
table(originial_moody[originial_moody$Score >= 46.5 
                      & originial_moody$Score < 66.5
                      & originial_moody$Seniority != 'Freshman',]$Grade)#A-0,B-3,C-11,D-1,F-0
table(originial_moody[originial_moody$Score >= 46.5 
                      & originial_moody$Score < 66.5
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major == 'Psychology',]$Grade)#A-0,B-10,C-11,D-0,F-0
table(originial_moody[originial_moody$Score >= 56.5 
                      & originial_moody$Score < 66.5
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major == 'Psychology',]$Grade)#A-0,B-9,C-1,D-0,F-0  (Pick this one)
table(originial_moody[originial_moody$Score >= 46.5 
                      & originial_moody$Score < 56.5
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major == 'Psychology',]$Grade)#A-0,B-1,C-10,D-0,F-0 (PIck this one)
table(originial_moody[originial_moody$Score >= 46.5 
                      & originial_moody$Score < 66.5
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major != 'Psychology',]$Grade)#A-0,B-1,C-40,D-23,F-6
table(originial_moody[originial_moody$Score >= 55.5 
                      & originial_moody$Score < 66.5
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major != 'Psychology',]$Grade)#A-0,B-1,C-36,D-7,F-2 (Pick this one)
table(originial_moody[originial_moody$Score >= 46.5 
                      & originial_moody$Score < 55.5
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major != 'Psychology',]$Grade)#A-0,B-0,C-4,D-16,F-4 (Pick this one)
table(originial_moody[originial_moody$Score >= 0 
                      & originial_moody$Score < 46.5,]$Grade)#A-3,B-2,C-19,D-71,F-188
table(originial_moody[originial_moody$Score >= 0 
                      & originial_moody$Score < 46.5
                      & originial_moody$Seniority == 'Freshman',]$Grade)#A-2,B-1,C-12,D-55,F-1  (Pick this one)
table(originial_moody[originial_moody$Score >= 0 
                      & originial_moody$Score < 46.5
                      & originial_moody$Seniority != 'Freshman',]$Grade)#A-1,B-1,C-7,D-16,F-187 (Pick this one)
###########   1st attempt #################
###########   2nd attempt ################
table(originial_moody[originial_moody$Score >= 87.5 
                      & originial_moody$Score <= 100,]$Grade)#A-143,B-7,C-1,D-1,F-1 (Pick this one)
table(originial_moody[originial_moody$Score >= 70.5 & originial_moody$Score < 87.5 
                      & (originial_moody$Major == 'Economics' 
                         | originial_moody$Major == 'Psychology'),]$Grade)#A-35,B-7,C-1,D-0,F-0  (Pick this one)
table(originial_moody[originial_moody$Score >= 66.5 & originial_moody$Score < 70.5 
                      & (originial_moody$Major == 'Economics' 
                         | originial_moody$Major == 'Psychology'),]$Grade)#A-5,B-8,C-0,D-0,F-0  (Pick this one)
table(originial_moody[originial_moody$Score >= 77 & originial_moody$Score < 87.5 
                      & (originial_moody$Major == 'CS' 
                         | originial_moody$Major == 'Statistics') 
                      & originial_moody$Seniority == 'Freshman',]$Grade)#A-13,B-3 (Pick this one)
table(originial_moody[originial_moody$Score >= 66.5 & originial_moody$Score < 77 
                      & (originial_moody$Major == 'CS' 
                         | originial_moody$Major == 'Statistics') 
                      & originial_moody$Seniority == 'Freshman',]$Grade)#A-0,B-10 (Pick this one)
table(originial_moody[originial_moody$Score >= 80.5 & originial_moody$Score < 87.5 
                      & (originial_moody$Major == 'CS' 
                         | originial_moody$Major == 'Statistics') 
                      & originial_moody$Seniority != 'Freshman',]$Grade)#B-27,C-1 (Pick this one)
table(originial_moody[originial_moody$Score >= 66.5 & originial_moody$Score < 80.5 
                      & (originial_moody$Major == 'CS' 
                         | originial_moody$Major == 'Statistics') 
                      & originial_moody$Seniority != 'Freshman',]$Grade)#B-5,C-22 (Pick this one)

table(originial_moody[originial_moody$Score >= 58.5 
                      & originial_moody$Score < 66.5
                      & originial_moody$Seniority == 'Freshman',]$Grade)#A-5,B-3,C-0,D-0,F-0  (Pick this one)
table(originial_moody[originial_moody$Score >= 46.5 
                      & originial_moody$Score < 58.5
                      & originial_moody$Seniority == 'Freshman'
                      & (originial_moody$Major == 'Economics' 
                         | originial_moody$Major == 'Psychology'),]$Grade)#A-0,B-10,C-3,D-0,F-0 (Pick this one)
table(originial_moody[originial_moody$Score >= 46.5 
                      & originial_moody$Score < 58.5
                      & originial_moody$Seniority == 'Freshman'
                      & (originial_moody$Major == 'CS' 
                         | originial_moody$Major == 'Statistics'),]$Grade)#A-0,B-3,C-11,D-1,F-0 (Pick this one)
table(originial_moody[originial_moody$Score >= 56.5 
                      & originial_moody$Score < 66.5
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major == 'Psychology',]$Grade)#A-0,B-9,C-1,D-0,F-0  (Pick this one)
table(originial_moody[originial_moody$Score >= 46.5 
                      & originial_moody$Score < 56.5
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major == 'Psychology',]$Grade)#A-0,B-1,C-10,D-0,F-0 (PIck this one)
table(originial_moody[originial_moody$Score >= 55.5 
                      & originial_moody$Score < 66.5
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major != 'Psychology',]$Grade)#A-0,B-1,C-36,D-7,F-2 (Pick this one)
table(originial_moody[originial_moody$Score >= 46.5 
                      & originial_moody$Score < 55.5
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major != 'Psychology',]$Grade)#A-0,B-0,C-4,D-16,F-4 (Pick this one)
table(originial_moody[originial_moody$Score >= 0 
                      & originial_moody$Score < 46.5
                      & originial_moody$Seniority == 'Freshman',]$Grade)#A-2,B-1,C-12,D-55,F-1  (Pick this one)
table(originial_moody[originial_moody$Score >= 31.5 
                      & originial_moody$Score < 46.5
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major == 'Psychology',]$Grade)#A-1,B-0,C-2,D-9,F-0  (Pick this one)
table(originial_moody[originial_moody$Score >= 31.5 
                      & originial_moody$Score < 46.5
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major != 'Psychology',]$Grade)#A-0,B-0,C-1,D-5,F-58  (Pick this one)
table(originial_moody[originial_moody$Score >= 0 
                      & originial_moody$Score < 31.5
                      & originial_moody$Seniority != 'Freshman',]$Grade)#A-0,B-1,C-4,D-2,F-129  (Pick this one)
######### 2nd attempt ###########################
######### 3rd attempt #######################
model1 <- rpart(Grade ~ Major + Score + Seniority, data = originial_moody[originial_moody$Score>67,], 
                method = "class", control = rpart.control(cp = 0.005))
model2 <- rpart(Grade ~ Major + Score + Seniority, data = originial_moody[originial_moody$Score>33 & originial_moody$Score<=67,], 
                method = "class", control = rpart.control(cp = 0.005))
model3 <- rpart(Grade ~ Major + Score + Seniority, data = originial_moody[originial_moody$Score<=33,], 
                method = "class", control = rpart.control(cp = 0.005))

model1
model2
model3
#pred1 <- predict(model1, data=originial_moody[originial_moody$Score>67,], type="class",
#                 control = rpart.control(cp = 0.005))
#pred2 <- predict(model2, data=originial_moody[originial_moody$Score>33 & originial_moody$Score<=67,], type="class",
#                 control = rpart.control(cp = 0.005))
#pred3 <- predict(model3, data=originial_moody[originial_moody$Score<=33,], type="class",
#                 control = rpart.control(cp = 0.005))
pred1 <- predict(model1, newdata=missing_moody, type="class",
                 control = rpart.control(cp = 0.005))
pred2 <- predict(model2, newdata=missing_moody, type="class",
                 control = rpart.control(cp = 0.005))
pred3 <- predict(model3, newdata=missing_moody, type="class",
                 control = rpart.control(cp = 0.005))
myprediction <- originial_moody
myprediction <- missing_moody
decision <- rep('F',nrow(myprediction))
decision[myprediction$Score>67] <- as.character(pred1)
decision[myprediction$Score>33 & myprediction$Score<=67] <-as.character(pred2)
decision[myprediction$Score<=33] <-as.character(pred3)

myprediction$Grade <-decision
error <- mean(originial_moody$Grade != myprediction$Grade)
error

####### 3rd attempt ######################
######## Cross Validation #################
#install.packages("devtools") 
#devtools::install_github("devanshagr/CrossValidation")
CrossValidation::cross_validate(originial_moody,model1,10,0.7)
CrossValidation::cross_validate(originial_moody,model2,10,0.7)
CrossValidation::cross_validate(originial_moody,model3,10,0.7)
####### Submission ############
originial_moody <- read.csv('M2022train.csv')
tail(originial_moody)
missing_moody <- read.csv('M2022testSNoGrade.csv')
tail(missing_moody)
submission2 <- read.csv('M2022submission.csv')
head(submission2)
myprediction <- missing_moody

# All of the decision vectors go here ###

submission2$Grade <- decision
write.csv(submission2, 'submission2.csv', row.names=FALSE)

######################
######################
summary(originial_moody)
myprediction <- originial_moody
decision <- rep('F',nrow(myprediction))
#decision[myprediction$Score >= 66.5 & myprediction$Score <= 100] <- 'A'
decision[myprediction$Score >= 87.5 & myprediction$Score <= 100] <- 'A'
#decision[myprediction$Score >= 66.5 & myprediction$Score < 87.5] <- 'B'
#decision[myprediction$Score >= 66.5 & myprediction$Score < 87.5 & 
#           (myprediction$Major == 'Economics' | myprediction$Major == 'Psychology')] <- 'A'
decision[myprediction$Score >= 70.5 & myprediction$Score < 87.5 & 
           (myprediction$Major == 'Economics' | myprediction$Major == 'Psychology')] <- 'A'
decision[myprediction$Score >= 66.5 & myprediction$Score < 70.5 & 
           (myprediction$Major == 'Economics' | myprediction$Major == 'Psychology')] <- 'B'
#decision[myprediction$Score >= 66.5 & myprediction$Score < 87.5 & 
#           (myprediction$Major == 'CS' | myprediction$Major == 'Statistics')] <- 'B'
#decision[myprediction$Score >= 66.5 & myprediction$Score < 87.5 & 
#           (myprediction$Major == 'CS' | myprediction$Major == 'Statistics') 
#            & myprediction$Seniority == 'Freshman'] <- 'A'
decision[myprediction$Score >= 77 & myprediction$Score < 87.5 & 
           (myprediction$Major == 'CS' | myprediction$Major == 'Statistics') 
         & myprediction$Seniority == 'Freshman'] <- 'A'
decision[myprediction$Score >= 66.5 & myprediction$Score < 77 & 
           (myprediction$Major == 'CS' | myprediction$Major == 'Statistics') 
         & myprediction$Seniority == 'Freshman'] <- 'B'
#decision[myprediction$Score >= 66.5 & myprediction$Score < 87.5 & 
#           (myprediction$Major == 'CS' | myprediction$Major == 'Statistics') 
#         & myprediction$Seniority != 'Freshman'] <- 'B'
decision[myprediction$Score >= 80.5 & myprediction$Score < 87.5 & 
           (myprediction$Major == 'CS' | myprediction$Major == 'Statistics') 
         & myprediction$Seniority != 'Freshman'] <- 'B'
decision[myprediction$Score >= 66.5 & myprediction$Score < 80.5 & 
           (myprediction$Major == 'CS' | myprediction$Major == 'Statistics') 
         & myprediction$Seniority != 'Freshman'] <- 'C'

#decision[myprediction$Score >= 0 & myprediction$Score < 66.5] <- 'F'
#decision[myprediction$Score >= 46.5 & myprediction$Score < 66.5] <- 'C'
#decision[myprediction$Score >= 46.5 & myprediction$Score < 66.5 
#         & myprediction$Seniority == 'Freshman'] <- 'B'
decision[myprediction$Score >= 58.5 & myprediction$Score < 66.5 
         & myprediction$Seniority == 'Freshman'] <- 'A'
#decision[myprediction$Score >= 46.5 & myprediction$Score < 58.5 
#         & myprediction$Seniority == 'Freshman'] <- 'C'
decision[myprediction$Score >= 46.5 & myprediction$Score < 58.5 
         & myprediction$Seniority == 'Freshman'
         & (myprediction$Major == 'Economics' | myprediction$Major == 'Psychology')] <- 'B'
decision[myprediction$Score >= 46.5 & myprediction$Score < 58.5 
         & myprediction$Seniority == 'Freshman'
         & (myprediction$Major == 'CS' | myprediction$Major == 'Statistics')] <- 'C'
#decision[myprediction$Score >= 46.5 & myprediction$Score < 66.5
#         & myprediction$Seniority != 'Freshman'] <- 'C'
#decision[myprediction$Score >= 46.5 & myprediction$Score < 66.5
#         & myprediction$Seniority != 'Freshman'
#         & myprediction$Major == 'Psychology'] <- 'C'
decision[myprediction$Score >= 56.5 & myprediction$Score < 66.5
         & myprediction$Seniority != 'Freshman'
         & myprediction$Major == 'Psychology'] <- 'B'
decision[myprediction$Score >= 46.5 & myprediction$Score < 56.5
         & myprediction$Seniority != 'Freshman'
         & myprediction$Major == 'Psychology'] <- 'C'
#decision[myprediction$Score >= 46.5 & myprediction$Score < 66.5
#         & myprediction$Seniority != 'Freshman'
#         & myprediction$Major != 'Psychology'] <- 'C'
decision[myprediction$Score >= 55.5 & myprediction$Score < 66.5
         & myprediction$Seniority != 'Freshman'
         & myprediction$Major != 'Psychology'] <- 'C'
decision[myprediction$Score >= 46.5 & myprediction$Score < 55.5
         & myprediction$Seniority != 'Freshman'
         & myprediction$Major != 'Psychology'] <- 'D'

#decision[myprediction$Score >= 0 & myprediction$Score < 46.5] <- 'F'
decision[myprediction$Score >= 0 & myprediction$Score < 46.5 
         & myprediction$Seniority == 'Freshman'] <- 'D'
#decision[myprediction$Score >= 0 & myprediction$Score < 46.5 
#         & myprediction$Seniority != 'Freshman'] <- 'F'
#decision[myprediction$Score >= 31.5 & myprediction$Score < 46.5 
#         & myprediction$Seniority != 'Freshman'] <- 'F'
decision[myprediction$Score >= 31.5 & myprediction$Score < 46.5 
         & myprediction$Seniority != 'Freshman'
         & myprediction$Major == 'Psychology'] <- 'D'
decision[myprediction$Score >= 31.5 & myprediction$Score < 46.5 
         & myprediction$Seniority != 'Freshman'
         & myprediction$Major != 'Psychology'] <- 'F'
decision[myprediction$Score >= 0 & myprediction$Score < 31.5 
         & myprediction$Seniority != 'Freshman'] <- 'F'

myprediction$Grade <- decision

error <- mean(myprediction$Grade != originial_moody$Grade)
error

##########
