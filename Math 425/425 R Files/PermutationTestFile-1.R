
#Be sure to run this first!
set.seed(151)

#Then run all other codes in order, once
#immediately after running that code.


#Step 1
myTest <- glm(Temp > 95 ~ Wind, data=airquality, family=binomial)
observedTestStat <-  summary(myTest)$coefficients[2, "z value"]

#Step 2
N <- 2000      
permutedTestStats <- rep(NA, N)
for (i in  1:N){
  permutedData <- sample(airquality$Temp > 95)
  permutedTest <- glm(permutedData ~ Wind, data=airquality, family=binomial)
  permutedTestStats[i] <- summary(permutedTest)$coefficients[2, "z value"]
}
hist(permutedTestStats)
abline(v=observedTestStat)

#Step 3
sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N
