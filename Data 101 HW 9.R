# Data 101 HW 9
setwd('C:/Users/Owner/Downloads/Data 101 R Assignments')
originial_moody <- read.csv('M2022train.csv')
head(originial_moody)
missing_moody <- read.csv('M2022testS.csv')
head(missing_moody)
colors <- c('green','blue','cyan','yellow','red','orange')

table(originial_moody$Grade)
barplot(table(originial_moody$Grade),xlab = 'Grade', ylab = 'Frequency',col = colors)
boxplot(originial_moody$Score ~ originial_moody$Grade, 
        xlab = 'Grade', ylab = 'Score', col = 'orange') # A > B > C > D > F
mosaicplot(originial_moody$Major~originial_moody$Grade, xlab = 'Major', ylab = 'Grade',
           main = 'Grade Distribution based on Major',col = colors)
mosaicplot(originial_moody$Seniority~originial_moody$Grade, xlab = 'Seniority', ylab = 'Grade',
           main = 'Grade Distribution based on Seniority',col = colors)
### 1st prediction model ###
table(originial_moody[originial_moody$Seniority == 'Freshman',]$Grade) # A-71, B-30, C-26, D-56, F-2
table(originial_moody[originial_moody$Seniority == 'Sophomore',]$Grade) # A-47, B-24, C-21, D-14, F-59
table(originial_moody[originial_moody$Seniority == 'Junior',]$Grade)  # A-36, B-23, C-29, D-9, F-67
table(originial_moody[originial_moody$Seniority == 'Senior',]$Grade) # A-50, B-19, C-33, D-17, F-67

table(originial_moody[originial_moody$Major == 'CS',]$Grade) # A- 53, B-38, C-30, D-34, F-49
table(originial_moody[originial_moody$Major == 'Economics',]$Grade) # A- 43, B-19, C-22, D-23, F-61
table(originial_moody[originial_moody$Major == 'Psychology',]$Grade) # A- 55, B-18, C-23, D-18, F-38
table(originial_moody[originial_moody$Major == 'Statistics',]$Grade) # A- 53, B-21, C-34, D-21, F-47

table(originial_moody[originial_moody$Score >= 90,]$Grade) # A- 133, B-4, C-1, D-1, F-0 
table(originial_moody[originial_moody$Score >= 80,]$Grade) # A-172, B-35, C-6, D-1, F-1
table(originial_moody[originial_moody$Score <= 65,]$Grade) # A- 6, B-27, C-79, D-95, F-193 
table(originial_moody[originial_moody$Score >= 50 
                      & originial_moody$Score <= 80,]$Grade) # A- 31, B-57, C-81, D-20, F-4
table(originial_moody[originial_moody$Score <= 40,]$Grade) # A- 2, B-2, C-14, D-59, F-171

### 2nd prediction model ###
table(originial_moody[originial_moody$Score >= 88 
                      & originial_moody$Major == 'CS',]$Grade) # A-48, B-7, D-1
table(originial_moody[originial_moody$Score >= 90 
                      & originial_moody$Major == 'Economics',]$Grade) # A-21
table(originial_moody[originial_moody$Score >= 90 
                      & originial_moody$Major == 'Psychology',]$Grade) # A-27, C-1
table(originial_moody[originial_moody$Score >= 90 
                      & originial_moody$Major == 'Statistics',]$Grade) # A-39

table(originial_moody[originial_moody$Score >= 88 & originial_moody$Score <= 100
                      & originial_moody$Major == 'CS',]$Grade) # A-48, B-7, D-1
table(originial_moody[originial_moody$Score >= 74 & originial_moody$Score <= 100 
                      & originial_moody$Major == 'Economics',]$Grade) # A-36,B-4, F-1 
table(originial_moody[originial_moody$Score >= 70 & originial_moody$Score <= 100
                      & originial_moody$Major == 'Psychology',]$Grade) # A-48,B-1,C-2
table(originial_moody[originial_moody$Score >= 86 & originial_moody$Score <= 100
                      & originial_moody$Major == 'Statistics',]$Grade) # A-45

###### 3rd prediction model ######
##        CS Model  ####
table(originial_moody[originial_moody$Score >= 51 & originial_moody$Score < 88
                      & originial_moody$Major == 'CS',]$Grade) # A-4,B-31,C-28,D-14,F-1

table(originial_moody[originial_moody$Score > 81 & originial_moody$Score < 88
                      & originial_moody$Major == 'CS',]$Grade) # A-4,B-15,C-1
table(originial_moody[originial_moody$Score > 81 & originial_moody$Score < 88
                      & originial_moody$Seniority == 'Freshman'
                      & originial_moody$Major == 'CS',]$Grade) # A-4
table(originial_moody[originial_moody$Score > 81 & originial_moody$Score < 88
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major == 'CS',]$Grade) # B-15,C-1
table(originial_moody[originial_moody$Score >= 51 & originial_moody$Score < 81
                      & originial_moody$Major == 'CS',]$Grade) # B-14,C-27,D-14,F-1

table(originial_moody[originial_moody$Score >= 64 & originial_moody$Score < 81
                      & originial_moody$Major == 'CS',]$Grade) #B-13, C-17, F-1
table(originial_moody[originial_moody$Score >= 64 & originial_moody$Score < 81
                      & originial_moody$Seniority == 'Freshman'
                      & originial_moody$Major == 'CS',]$Grade) #B-12
table(originial_moody[originial_moody$Score >= 64 & originial_moody$Score < 81
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major == 'CS',]$Grade) #B-1, C-17, D-1
table(originial_moody[originial_moody$Score >= 61 & originial_moody$Score < 64
                      & originial_moody$Major == 'CS',]$Grade) # B-1, C-4
table(originial_moody[originial_moody$Score >= 51 & originial_moody$Score < 61
                      & originial_moody$Major == 'CS',]$Grade) # C-6, D-14

table(originial_moody[originial_moody$Score >= 0 & originial_moody$Score < 51
                      & originial_moody$Major == 'CS',]$Grade) # A-1, C-2, D-19, F-48
table(originial_moody[originial_moody$Score >= 0 & originial_moody$Score < 51
                      & originial_moody$Major == 'CS'
                      & originial_moody$Seniority == 'Freshman',]$Grade) # A-1, C-1, D-19, F-1
table(originial_moody[originial_moody$Score >= 0 & originial_moody$Score < 51
                      & originial_moody$Major == 'CS'
                      & originial_moody$Seniority != 'Freshman',]$Grade) # C-1, F-47

### Econ Model ####
table(originial_moody[originial_moody$Score >= 45 & originial_moody$Score < 74 
                      & originial_moody$Major == 'Economics',]$Grade)# A-7,B-14,C-19,D-3,F-1
table(originial_moody[originial_moody$Score >= 66 & originial_moody$Score < 74 
                      & originial_moody$Major == 'Economics',]$Grade) #A-7,B-8
table(originial_moody[originial_moody$Score >= 66 & originial_moody$Score < 74
                      & originial_moody$Seniority == 'Freshman'
                      & originial_moody$Major == 'Economics',]$Grade) #A-7
table(originial_moody[originial_moody$Score >= 66 & originial_moody$Score < 74
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major == 'Economics',]$Grade) #B-8

table(originial_moody[originial_moody$Score >= 45 & originial_moody$Score < 66 
                      & originial_moody$Major == 'Economics',]$Grade) # B-6,C-19,D-3.F-1
table(originial_moody[originial_moody$Score >= 0 & originial_moody$Score < 45 
                      & originial_moody$Major == 'Economics',]$Grade) # B-1,C-3,D-20,F-59
table(originial_moody[originial_moody$Score >= 0 & originial_moody$Score < 45 
                      & originial_moody$Major == 'Economics'
                      & originial_moody$Seniority == 'Freshman',]$Grade) # B-1,C-2,D-17
table(originial_moody[originial_moody$Score >= 0 & originial_moody$Score < 45 
                      & originial_moody$Major == 'Economics'
                      & originial_moody$Seniority != 'Freshman',]$Grade) # C-1, D-3,F-59

### Psychology Model ###
table(originial_moody[originial_moody$Score >= 32 & originial_moody$Score < 70
                      & originial_moody$Major == 'Psychology',]$Grade) # A-7,B-17,C-21,D-9
table(originial_moody[originial_moody$Score >= 61 & originial_moody$Score < 70
                      & originial_moody$Major == 'Psychology',]$Grade) # A-4, B-7
table(originial_moody[originial_moody$Score >= 61 & originial_moody$Score < 70
                      & originial_moody$Seniority == 'Freshman'
                      & originial_moody$Major == 'Psychology',]$Grade) #A-4
table(originial_moody[originial_moody$Score >= 61 & originial_moody$Score < 70
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major == 'Psychology',]$Grade) #B-7

table(originial_moody[originial_moody$Score >= 32 & originial_moody$Score < 61
                      & originial_moody$Major == 'Psychology',]$Grade) # A-3,B-10,C-21,D-9
table(originial_moody[originial_moody$Score >= 39 & originial_moody$Score < 61
                      & originial_moody$Major == 'Psychology',]$Grade) # A-2, B-10, C-13, D-1
table(originial_moody[originial_moody$Score >= 32 & originial_moody$Score < 39
                      & originial_moody$Seniority == 'Freshman'
                      & originial_moody$Major == 'Psychology',]$Grade) # C-8
table(originial_moody[originial_moody$Score >= 32 & originial_moody$Score < 39
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major == 'Psychology',]$Grade) # A-1, D-8

table(originial_moody[originial_moody$Score >= 0 & originial_moody$Score < 32
                      & originial_moody$Major == 'Psychology',]$Grade) # A-0,C-0, D-9, F-38
table(originial_moody[originial_moody$Score >= 0 & originial_moody$Score < 32
                      & originial_moody$Major == 'Psychology'
                      & originial_moody$Seniority == 'Freshman',]$Grade) # D-8
table(originial_moody[originial_moody$Score >= 0 & originial_moody$Score < 32
                      & originial_moody$Major == 'Psychology'
                      & originial_moody$Seniority != 'Freshman',]$Grade) # D-1, F-38

### Statistics Model ####
table(originial_moody[originial_moody$Score >= 51 & originial_moody$Score < 86
                      & originial_moody$Major == 'Statistics',]$Grade) # A-8, B-20, C-30, D-5
table(originial_moody[originial_moody$Score >= 76 & originial_moody$Score < 86
                      & originial_moody$Major == 'Statistics',]$Grade) # A-8,B-14,C-1
table(originial_moody[originial_moody$Score >= 76 & originial_moody$Score < 86
                      & originial_moody$Seniority == 'Freshman'
                      & originial_moody$Major == 'Statistics',]$Grade) #A-8
table(originial_moody[originial_moody$Score >= 76 & originial_moody$Score < 86
                      & originial_moody$Seniority != 'Freshman'
                      & originial_moody$Major == 'Statistics',]$Grade) #B-14, C-1
table(originial_moody[originial_moody$Score >= 51 & originial_moody$Score < 76
                      & originial_moody$Major == 'Statistics',]$Grade) # B-6,C-29,D-5

table(originial_moody[originial_moody$Score >= 0 & originial_moody$Score < 51
                      & originial_moody$Major == 'Statistics',]$Grade) # B-1, C-4, D-16,F-47
table(originial_moody[originial_moody$Score >= 0 & originial_moody$Score < 51
                      & originial_moody$Major == 'Statistics'
                      & originial_moody$Seniority == 'Freshman',]$Grade) #C-1, D-12
table(originial_moody[originial_moody$Score >= 0 & originial_moody$Score < 51
                      & originial_moody$Major == 'Statistics'
                      & originial_moody$Seniority != 'Freshman',]$Grade) #B-1, C-3,D-4,F-47

## Actual Prediction Model ###
summary(originial_moody)
myprediction <- originial_moody
decision <- rep('F',nrow(myprediction))
decision[myprediction$Score >= 88 & myprediction$Score <= 100 & myprediction$Major == 'CS'] <- 'A'
decision[myprediction$Score >= 91 & myprediction$Score <= 100 & myprediction$Seniority == 'Senior' 
         & myprediction$Major == 'CS'] <- 'A'
decision[myprediction$Score >= 88 & myprediction$Score < 91 & myprediction$Seniority == 'Senior' 
         & myprediction$Major == 'CS'] <- 'B'
decision[myprediction$Score >= 74 & myprediction$Score <= 100 & myprediction$Major == 'Economics'] <- 'A'
decision[myprediction$Score >= 70 & myprediction$Score <= 100 & myprediction$Major == 'Psychology'] <- 'A'
decision[myprediction$Score >= 86 & myprediction$Score <= 100 & myprediction$Major == 'Statistics'] <- 'A'

decision[myprediction$Score >= 81 & myprediction$Score < 88 & myprediction$Seniority == 'Freshman'
         & myprediction$Major == 'CS'] <- 'A'
decision[myprediction$Score >= 81 & myprediction$Score < 88 & myprediction$Seniority != 'Freshman' 
         & myprediction$Major == 'CS'] <- 'B'
decision[myprediction$Score >= 66 & myprediction$Score < 74 & myprediction$Seniority == 'Freshman'
         & myprediction$Major == 'Economics'] <- 'A'
decision[myprediction$Score >= 66 & myprediction$Score < 74 & myprediction$Seniority != 'Freshman'
         & myprediction$Major == 'Economics'] <- 'B'
decision[myprediction$Score >= 61 & myprediction$Score < 70 & myprediction$Seniority == 'Freshman' 
         & myprediction$Major == 'Psychology'] <- 'A'
decision[myprediction$Score >= 61 & myprediction$Score < 70 & myprediction$Seniority != 'Freshman'
         & myprediction$Major == 'Psychology'] <- 'B'
decision[myprediction$Score >= 76 & myprediction$Score < 86 & myprediction$Seniority == 'Freshman'
         & myprediction$Major == 'Statistics'] <- 'A'
decision[myprediction$Score >= 76 & myprediction$Score < 86 & myprediction$Seniority != 'Freshman'
         & myprediction$Major == 'Statistics'] <- 'B'

decision[myprediction$Score >= 64 & myprediction$Score < 81 & myprediction$Seniority == 'Freshman'
         & myprediction$Major == 'CS'] <- 'B'
decision[myprediction$Score >= 64 & myprediction$Score < 81 & myprediction$Seniority != 'Freshman'
         & myprediction$Major == 'CS'] <- 'C'
decision[myprediction$Score >= 61 & myprediction$Score < 64 & myprediction$Major == 'CS'] <- 'C'
decision[myprediction$Score >= 51 & myprediction$Score < 61 & myprediction$Major == 'CS'] <- 'D'

decision[myprediction$Score >= 45 & myprediction$Score < 66 & myprediction$Major == 'Economics'] <- 'C'
decision[myprediction$Score >= 39 & myprediction$Score < 61 & myprediction$Major == 'Psychology'] <- 'C'
decision[myprediction$Score >= 32 & myprediction$Score < 39 & myprediction$Seniority == 'Freshman'
         & myprediction$Major == 'Psychology'] <- 'C'
decision[myprediction$Score >= 32 & myprediction$Score < 39 & myprediction$Seniority != 'Freshman'
         & myprediction$Major == 'Psychology'] <- 'D'
decision[myprediction$Score >= 51 & myprediction$Score < 76 & myprediction$Major == 'Statistics'] <- 'C'

decision[myprediction$Score >= 0 & myprediction$Score < 51 & myprediction$Seniority == 'Freshman' 
         & myprediction$Major == 'CS'] <- 'D'
decision[myprediction$Score >= 0 & myprediction$Score < 45 & myprediction$Seniority == 'Freshman' 
         & myprediction$Major == 'Economics'] <- 'D'
decision[myprediction$Score >= 0 & myprediction$Score < 32 & myprediction$Seniority == 'Freshman' 
         & myprediction$Major == 'Psychology'] <- 'D'
decision[myprediction$Score >= 0 & myprediction$Score < 51 & myprediction$Seniority == 'Freshman' 
         & myprediction$Major == 'Statistics'] <- 'D'
decision[myprediction$Score >= 0 & myprediction$Score < 51 & myprediction$Seniority != 'Freshman' 
         & myprediction$Major == 'CS'] <- 'F'
decision[myprediction$Score >= 0 & myprediction$Score < 45 & myprediction$Seniority != 'Freshman' 
         & myprediction$Major == 'Economics'] <- 'F'
decision[myprediction$Score >= 0 & myprediction$Score < 32 & myprediction$Seniority != 'Freshman' 
         & myprediction$Major == 'Psychology'] <- 'F'
decision[myprediction$Score >= 0 & myprediction$Score < 51 & myprediction$Seniority != 'Freshman' 
         & myprediction$Major == 'Statistics'] <- 'F'
myprediction$Grade <- decision
error <- mean(originial_moody$Grade != myprediction$Grade)
error

#############     Official Submission  ################
missing_moody <- read.csv('M2022testS.csv')
submission <- read.csv('M2022submission.csv')
myprediction <- missing_moody

# Next step: All of the decision vectors go here ###

submission$Grade <- decision
head(submission)

# write csv file
write.csv(submission, 'submission.csv', row.names = FALSE)

###############################


