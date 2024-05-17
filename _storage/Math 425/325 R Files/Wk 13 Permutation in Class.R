# Permutation in class
library(mosaic)
library(tidyverse)


# First run the initial test and gain the test statistic:
myTest <- t.test(extra ~ group, data = sleep, mu = 0)
# Running a t test, saving test statistic, saving it for the t equation
observedTestStat <- myTest$statistic

# Now we run the permutations to create a distribution of test statistics
# Create a data structure to hold numbers in the future
# Permutedteststats is just 2000 empty cubbies  
# Sample(extra) takes a random group from

N <- 2000
permutedTestStats <- rep(NA, N)
for (i in 1:N){
  permutedTest <- t.test(sample(extra) ~ group, data = sleep, mu = 0)
  permutedTestStats[i] <- permutedTest$statistic
}

# Now we show a histogram of that distribution
hist(permutedTestStats, col = "skyblue")
abline(v = observedTestStat, col = "red", lwd = 3)

#Greater-Than p-value: Not the correct one in this case
sum(permutedTestStats >= observedTestStat)/N

# Less-Than p-value: Not the correct one for this data
sum(permutedTestStats <= observedTestStat)/N

# Two-Sided p-value: This is the one we want based on our alternative hypothesis.
# Assumed symmetry?
2*sum(permutedTestStats <= observedTestStat)/N

# Distribution is different each time because of RNG