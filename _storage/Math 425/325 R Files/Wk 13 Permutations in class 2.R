# Permutation 2

# Perform an independent two-sample t test using a permutation test.
# Use the mtcars dataset and test whether the average weight of the four cylinder cars differs from the average weight of eight cylinder cars.  Use the following psuedo code as a starting spot.

library(dplyr)
library(mosaic)

?mtcars
View(mtcars)

# What does sample() do?

cbind(mtcars$wt, mtcars$cyl)
cbind(sample(mtcars$wt), mtcars$cyl)
cbind(mtcars$wt, sample(mtcars$cyl))

# set.seed(123)

# new_mtcars <- mtcars %>%
#  mutate(cylinderCount = case_when(
#    cyl == 8 ~ 8,
#    cyl == 4 ~ 4,
#    TRUE ~ NA_integer_  # You might want to handle other cases as well
#  ))

#new_mtcars = filter(mtcars, mtcars$cyl == 8 | mtcars$cyl == 4)
new_mtcars <- filter(mtcars, cyl == 8 | cyl == 4)
View(new_mtcars)

# Step 1: Perform t-test on original data
myTest <- t.test(wt ~ as.factor(cyl), data = new_mtcars)
observedTestStat <- myTest$statistic
print(observedTestStat)

# Step 2: Permutation test
N <- 2000
permutedTestStats <- rep(NA, N)

for (i in 1:N) {
  permutedData <- sample(new_mtcars$wt)
  permutedTest <- t.test(permutedData ~ as.factor(new_mtcars$cyl))
  permutedTestStats[i] <- permutedTest$statistic
}

# Need to change graph size with xlim
hist(permutedTestStats, main = "Permutation Test Results", xlab = "p-values", xlim = c(-7,7))
abline(v = observedTestStat, col = "red")

# Step 3: Compute p-value
p_value_greater <- sum(permutedTestStats >= observedTestStat) / N
p_value_less <- sum(permutedTestStats <= observedTestStat) / N

# Output the p-values
print(paste("p-value (greater):", p_value_greater))
print(paste("p-value (less):", p_value_less))

# Diamonds permutation test

View(diamonds)

#Step 1
myAOV <- aov(price ~ clarity, data = diamonds)
observedTestStat <- summary(myAOV)[[1]]$`F value`[1] #Accesses the f stat
print(observedTestStat)

#Step 2
N <- 100
permutedTestStats <- rep(NA, N)

for (i in  1:N){
  permutedData <- sample(diamonds$price)
  permutedTest <- aov(permutedData ~ as.factor(diamonds$clarity))
  permutedTestStats[i] <- summary(permutedTest)[[1]]$`F value`[1]
}

# F distribution starts at 0 !!
hist(permutedTestStats, xlim = c(0,220))
abline(v=observedTestStat)

#Step 3
sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N



# Logistic Probability

?SAT
SAT <- SAT %>% mutate(
  over1000 = case_when(
    sat > 1000 ~ 1,
    sat <= 1000 ~ 0
  )
)

#Step 1
myTest <- glm(over1000 ~ expend, data = SAT, family="binomial")
observedTestStat <- summary(myTest)[[12]][2,3]
print(observedTestStat)

#Step 2
N <- 100      
permutedTestStats <- rep(NA, N)
  for (i in  1:N){
  permutedData <- sample(SAT$expend)
  permutedTest <- glm(permutedData ~ as.factor(over1000), data = SAT)
  permutedTestStats[i] <- summary(permutedTest)[[12]][2,3]
}
hist(permutedTestStats,xlim = c(-3.5,3.5))
abline(v=observedTestStat,col="red")

#Step 3
sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N




set.seed(1140411) #allows us each to get the same set of random values
sample1 <- rnorm(30, 69, 2.5) #get a random sample of n=30 normally distributed values with mu=69 and sigma=2.5
sample2 <- rnorm(30, 69, 2.5) #get another random sample of n=30 normally distributed values with mu=69 and sigma=2.5
theData <- data.frame(values = c(sample1,sample2), group = rep(c(1,2), each=30)) #load the random samples into a data set
View(theData)
boxplot(values ~ group, data = theData)



set.seed(1140411)
sample1 <- rnorm(30, 69, 2.5)
sample2 <- rnorm(30, 69, 2.5)
theData <- data.frame(values = c(sample1,sample2), group = rep(c(1,2), each=30))
# View(theData)
boxplot(values ~ group, data = theData)



# Run the permutation test:

myTest <- t.test(values ~ group, data=theData, mu = 0)
observedTestStat <- myTest$statistic

observedTestStat



N <- 2000
permutedTestStats <-  rep(NA, N)
for  (i in 1:N ) {
  permutedTest <- t.test(sample(values) ~ group, data=theData, mu = 0)
  permutedTestStats[i]  <-  permutedTest$statistic
}
hist(permutedTestStats)
abline(v=observedTestStat)
sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N



# 3
set.seed(121) #ensure we each get the same set of random values
sample1 <- rnorm(30, 185, 8) #randomly sample n=30 normally distributed values with a mu=185 and sigma=8.
sample2 <- sample1 - rnorm(30, 0, 3.5) #randomly shift each value from sample1, but with mu=0 change overall.
theData <- data.frame(values = c(sample1,sample2), group = rep(c(1,2), each=30), id = rep(c(1:30),times=2)) #load the data into a data set
View(theData)
with(theData, hist(values[group==1] - values[group==2]))


# 4

# Create the data:

set.seed(121)
sample1 <- rnorm(30, 185, 8)
sample2 <- sample1 - rnorm(30, 0, 3.5)
theData <- data.frame(values = c(sample1,sample2), group = rep(c(1,2), each=30), id = rep(c(1:30),times=2))
# View(theData)
with(theData, hist(values[group==1] - values[group==2]))


# Perform the permutation test:

myTest <- t.test(values ~ group, data=theData, paired=TRUE, mu = 0)
observedTestStat <- myTest$statistic
observedTestStat

N <- 2000      
permutedTestStats <-  rep(NA, N)
for  (i in 1:N ) {
  permutedData <- sample(x=c(1,-1), size = 30, replace = TRUE)
  permutedTest <- with(theData, t.test(permutedData*(values[group==1]-values[group==2]),mu = 0))
  permutedTestStats[i]  <-  permutedTest$statistic
}
hist(permutedTestStats)
abline(v=observedTestStat)
sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N


# Review

Library(mosaic)
?SaratogaHouses
View(SaratogaHouses)
table(SaratogaHouses$fuel)


shkt <- kruskal.test(price ~ fuel, data = SaratogaHouses)
shkt

boxplot(price~fuel, data=SaratogaHouses)
stripchart(price ~ fuel, data=SaratogaHouses)
barchart(price ~ fuel, data=SaratogaHouses)

median(filter(SaratogaHouses, fuel == "gas")$price)


# 6
View(ToothGrowth)
?ToothGrowth

#its a miracle
tg.aov <- aov(len ~ as.factor(dose) + as.factor(supp) + as.factor(dose):as.factor(supp), data = ToothGrowth)
summary(tg.aov)

xyplot(len ~ dose, groups=supp, data=ToothGrowth, type=c("p","a"), auto.key=TRUE)


# Last one

wtrt <- wilcox.test(cloudcover ~ weekday, data = RailTrail)
wtrt
# NO df here


boxplot(cloudcover ~ weekday, data=RailTrail, names=c("Weekend/Holiday", "Weekday"), ylab="Cloud Cover Measurement (in oktas)")
