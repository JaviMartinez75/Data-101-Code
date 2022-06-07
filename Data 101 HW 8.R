# Data 101 HW 8
setwd('C:/Users/Owner/Downloads/Data 101 R Assignments')
minimarket <- read.csv('Minimarket.csv')
head(minimarket)
colors <- c('red','blue','cyan','yellow','green','orange')

table(minimarket$BREAD)    # 0: 4755     1: 4922
table(minimarket$BUTTER)   # 0: 4831     1: 4846
table(minimarket$COOKIES)  # 0: 4834     1: 4843
table(minimarket$COFFEE)   # 0: 4874     1: 4803
table(minimarket$TEA)      # 0: 4894     1: 4783


# Null: "Bread does not impact the sales of cookies"
# Alt: "Bread impacts the sale of cookies"
subset_bread_zero <- subset(minimarket, minimarket$BREAD == '0')
mean0cookies <- mean(subset_bread_zero$COOKIES)
sd0cookies <- sd(subset_bread_zero$COOKIES)
len0cookies <- length(subset_bread_zero$COOKIES)

subset_bread_one <- subset(minimarket, minimarket$BREAD == '1')
mean1cookies <- mean(subset_bread_one$COOKIES)
sd1cookies <- sd(subset_bread_one$COOKIES)
len1cookies <- length(subset_bread_one$COOKIES)

sd_0_1_cookies <- sqrt((sd0cookies^2)/len0cookies + (sd1cookies^2)/len1cookies)
zeta <- (mean0cookies - mean1cookies)/(sd_0_1_cookies)
zeta
p <- 1 - pnorm(zeta)
p # 0.005019

# Null: "Cookies does not impact the sales of tea"
# Alt: "Cookies impacts the sale of tea"
subset_cookies_zero <- subset(minimarket, minimarket$COOKIES == '0')
mean0tea <- mean(subset_cookies_zero$TEA)
sd0tea <- sd(subset_cookies_zero$TEA)
len0tea <- length(subset_cookies_zero$TEA)

subset_cookies_one <- subset(minimarket, minimarket$COOKIES == '1')
mean1tea <- mean(subset_cookies_one$TEA)
sd1tea <- sd(subset_cookies_one$TEA)
len1tea <- length(subset_cookies_one$TEA)

sd_0_1_tea <- sqrt((sd0tea^2)/len0tea + (sd1tea^2)/len1tea)
zeta <- (mean1tea - mean0tea)/(sd_0_1_tea)
zeta
p <- 1 - pnorm(zeta)
p # 0.006345

# Null: "Butter does not impact the sales of bread"
# Alt: "Butter impacts the sale of bread"
subset_butter_zero <- subset(minimarket, minimarket$BUTTER == '0')
mean0bread <- mean(subset_butter_zero$BREAD)
sd0bread <- sd(subset_butter_zero$BREAD)
len0bread <- length(subset_butter_zero$BREAD)

subset_butter_one <- subset(minimarket, minimarket$BUTTER == '1')
mean1bread <- mean(subset_butter_one$BREAD)
sd1bread <- sd(subset_butter_one$BREAD)
len1bread <- length(subset_butter_one$BREAD)

sd_0_1_bread <- sqrt((sd0bread^2)/len0bread + (sd1bread^2)/len1bread)
zeta <- (mean1bread - mean0bread)/(sd_0_1_bread)
zeta
p <- 1 - pnorm(zeta)
p # 0.06021



devtools::install_github("devanshagr/PermutationTestSecond")
mod3 <- rbind(subset_bread_zero, subset_bread_one)
PermutationTestSecond::Permutation(mod3, "BREAD", "BUTTER",10000,"1", "0") # Bad Pair
PermutationTestSecond::Permutation(mod3, "BREAD", "COOKIES",10000,"1", "0") # Good Pair
PermutationTestSecond::Permutation(mod3, "BREAD", "COFFEE",10000,"1", "0")  # Bad Pair
PermutationTestSecond::Permutation(mod3, "BREAD", "TEA",10000,"1", "0")     # Bad Pair

mod4 <- rbind(subset_butter_zero, subset_butter_one)
PermutationTestSecond::Permutation(mod4, "BUTTER", "BREAD",10000,"1", "0") # Bad Pair
PermutationTestSecond::Permutation(mod4, "BUTTER", "COOKIES",10000,"1", "0") # Bad Pair
PermutationTestSecond::Permutation(mod4, "BUTTER", "COFFEE",10000,"1", "0")  # Bad Pair
PermutationTestSecond::Permutation(mod4, "BUTTER", "TEA",10000,"1", "0")     # Bad Pair

subset_cookies_zero <- subset(minimarket, minimarket$COOKIES == '0')
subset_cookies_one <- subset(minimarket, minimarket$COOKIES == '1')
mod5 <- rbind(subset_cookies_zero, subset_cookies_one)
PermutationTestSecond::Permutation(mod5, "COOKIES", "BREAD",10000,"1", "0") # Good Pair
PermutationTestSecond::Permutation(mod5, "COOKIES", "BUTTER",10000,"1", "0") # Bad Pair
PermutationTestSecond::Permutation(mod5, "COOKIES", "COFFEE",10000,"1", "0")  # Bad Pair
PermutationTestSecond::Permutation(mod5, "COOKIES", "TEA",10000,"1", "0")     # Good Pair

subset_coffee_zero <- subset(minimarket, minimarket$COFFEE == '0')
subset_coffee_one <- subset(minimarket, minimarket$COFFEE == '1')
mod6 <- rbind(subset_coffee_zero, subset_coffee_one)
PermutationTestSecond::Permutation(mod6, "COFFEE", "BREAD",10000,"1", "0") # Bad Pair
PermutationTestSecond::Permutation(mod6, "COFFEE", "BUTTER",10000,"1", "0") # Bad Pair
PermutationTestSecond::Permutation(mod6, "COFFEE", "COOKIES",10000,"1", "0")  # Bad Pair
PermutationTestSecond::Permutation(mod6, "COFFEE", "TEA",10000,"1", "0")     # Bad Pair

subset_tea_zero <- subset(minimarket, minimarket$TEA == '0')
subset_tea_one <- subset(minimarket, minimarket$TEA == '1')
mod7 <- rbind(subset_tea_zero, subset_tea_one)
PermutationTestSecond::Permutation(mod7, "TEA", "BREAD",10000,"1", "0") # Bad Pair
PermutationTestSecond::Permutation(mod7, "TEA", "BUTTER",10000,"1", "0") # Bad Pair
PermutationTestSecond::Permutation(mod7, "TEA", "COOKIES",10000,"1", "0")  # Good Pair
PermutationTestSecond::Permutation(mod7, "TEA", "COFFEE",10000,"1", "0")     # Bad Pair
