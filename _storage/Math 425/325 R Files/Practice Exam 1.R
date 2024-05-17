# R practice test 1

library(dplyr)
library(mosaic)
library(car)
library(tidyverse)

# 1. Use an appropriate test and the starwars dataset in R to determine if, on average, the
# species of Wookiees, Gungans, or Kaminoans are taller. WRONG

?starwars
View(starwars)

# Assuming you have the starwars dataset loaded

# Calculate the average of a filtered column
avg_species <- starwars %>%
  filter(!is.na(species)) %>%  # Filter out NA values in the species column
  group_by(species) %>%        # Group by species
  summarise(avg_height = mean(height, na.rm = TRUE))  # Calculate the average height for each species

# Display the resulting table
print(avg_species)

wookie <- filter(starwars, species == "Wookiee")
# Calculate the average height of Wookiees
avg_height_wookie <- mean(wookie$height, na.rm = TRUE)
# Print the average height of Wookiees
print(avg_height_wookie)

gungan <- filter(starwars, species == "Gungan")
Kaminoan <- filter(starwars, species == "Kaminoan")
avg_height_kaminoan <- mean(Kaminoan$height, na.rm = TRUE)
avg_height_gungan <- mean(gungan$height, na.rm = TRUE)
print(avg_height_kaminoan)
print(avg_height_gungan)

# 2 Use the Highway1 dataset in R to answer this question.
# Suppose someone wanted to perform a t Test of the hypotheses
# H0:μ2 Lanes=μ4 Lanes
# Ha:μ2 Lanes≠μ4 Lanes
# where μ2 Lanes
# represents the 1973 accident rate (per million vehicle miles) on 2 lane highways, and μ4 Lanes
# represents the 1973 accident rate (per million vehicle miles) on 4 lane highways.
# Which of the following statements would be most correct concerning the appropriateness
# of performing this test for this data? WRONG

View(Highway1)
?Highway1


# 3 Is it appropriate to perform a simple linear regression on this data?
plot(height ~ age, data=Loblolly)
lob.lm <- lm(height ~ age, data=Loblolly)
b <- coef(lob.lm)
abline(lob.lm, add=TRUE)

plot(lob.lm, which=1:2)
plot(loblm$residuals, ylab="Residuals")
mtext("Residuals vs Order", side=3)

qqPlot(Loblolly$height)

# No, there is a strong non-linear pattern in the residual plot. <-


# Use the Highway1 dataset in R for this question.
# Compute the average accident rate (per million vehicle miles)
# in 1973 for each type of roadway: `MC`, `FAI`, `PA`, and `MA`.

# From cars
Highway1
?Highway1
View(Highway1)

average_accidents <- Highway1 %>%
  filter(!is.na(htype)) %>%  # Filter out NA values in the species column
  group_by(htype) %>%        # Group by species
  summarise(avg_accidents = mean(rate, na.rm = TRUE))  # Calculate the average height for each species

# Display the resulting table
print(average_accidents$avg_accidents)
View(average_accidents)


# Consider the barplot below and the hypothesis
# that gender and preference for DC or Marvel comics is independent.
# If this hypothesis is tested with an appropriate
# test using the data of this plot, what is the p-value of the test?



# In the KidsFeet dataset in R, there are three names of the
# children that occur twice, while all the other names occur
# only once. Compute the average foot width for the three names that
# occur more than once. Select the answer below that shows these
# three averages.

# Mosaic
?KidsFeet
View(KidsFeet)

# Caitlin, David, Josh

data("KidsFeet")

# Identify names that occur more than once
duplicate_names <- names(table(KidsFeet$name))[table(KidsFeet$name) > 1]

# Filter data for these names and calculate average foot width
average_widths <- sapply(duplicate_names, function(name) {
  mean(KidsFeet$width[KidsFeet$name == name])
})

# Print the averages
average_widths

# Use the iris dataset in R and the image below to come up
# with the regression equation for the versicolor species.

?iris
View(iris)
iris_versicolor <- filter(iris, Species == "versicolor")
versicolor_lm <- lm(Sepal.Length ~ Sepal.Width, data = iris_versicolor)
summary(versicolor_lm)

# 11
?cars
View(cars)
car.lm <- lm(speed ~ dist, data=cars)

# Predict the stopping distance for a given speed
speed_to_predict <- 130
predict(car.lm, data.frame(dist = 130))

# 12

?Galton

myTest <- t.test(height ~ sex, data=Galton, mu = 0)
myTest
observedTestStat <- myTest$statistic

observedTestStat

N <- 2000
permutedTestStats <-  rep(NA, N)
for  (i in 1:N ) {
  permutedTest <- t.test(sample(height) ~ sex, data=Galton, mu = 0)
  permutedTestStats[i]  <-  permutedTest$statistic
}
hist(permutedTestStats,xlim = c(-6,6))
abline(v=observedTestStat)
sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N

# 13

View(Highway1)
?Highway1
Highway2 <- subset(Highway1, lane == 2 | lane == 4)

wilcox.test(rate~lane,data=Highway2)

# Which set of plot labels would provide the most correct and most useful
# information for the graphic produced by this code?

plot(rate ~ slim, data=Highway1, pch=16, xlab="Speed Limit", ylab="Accident Rate", main="")

# 15 Use an appropriate test and graphic for the singer dataset in R (?singer)
# to determine if and how the median height differs for different voice parts.

?singer

kruskal.test(height ~ voice.part, data=singer)
boxplot(height ~ voice.part, data=singer)

#  What is the estimated probability that the speed of the car is greater than 15 mph if the car takes 70 feet to stop?

?cars

glmstop <- glm(dist ~ speed > 15, data=cars)
predict(glmstop, dist=70)
predicted_value <- predict(glmstop, newdata = data.frame(dist = 70), type = "response")
predicted_value
plot(speed > 15 ~ dist, data=cars, ylab="Probability Speed > 15 mph", xlab="Stopping Distance (feet)")


# Perform a two-way ANOVA using the KidsFeet dataset in R 
# that will allow you to test if the pattern shown in the following
# plot is real. Use the α=0.05 level and report the p-value of the test.
?KidsFeet

kf.aov <- aov(length ~ sex + domhand + sex:as.factor(domhand),data=KidsFeet)
kf.aov
summary(kf.aov)
xyplot(length ~ sex, data=KidsFeet, group=domhand, type=c("p","a"), auto.key=TRUE)

# Which set of statistics would be most meaningful to
# include in an analysis containing this histogram?

hist(islands, xlab="Area in Thousands of Square Miles", main="Areas of the World's Major Landmasses")


# make a graphic

?mtcars

hist(mtcars$cyl ~ mtcars$am)

barplot(table(mtcars$cyl, mtcars$am), 
        beside = TRUE, 
        legend.text = rownames(table(mtcars$cyl, mtcars$am)),
        col = c("red", "blue","green"),
        main = "Cylinders Count by Transmission Type",
        xlab = "Number of Cylinders",
        ylab = "Frequency")


# A logistic regression was performed to determine how cholesterol levels of adult men (X)
# impact their probability of having a heart attack (Y=1 denoting a heart attack)
# The value of β0 in the model was estimated to be 56.4146 while the value of β1
# in the model was estimated to be 0.251.
# Interpret the estimate for β1
# in context of the odds of an adult male having a heart attack.



# Calculate P values

f_statistic <- 15.36  # Your F-statistic
df1 <- 5           # Degrees of freedom for the numerator
df2 <- 65          # Degrees of freedom for the denominator

# Calculate the p-value
p_value <- pf(f_statistic, df1, df2, lower.tail = FALSE)

# Output the p-value
p_value


# Use an appropriate t Test and the ToothGrowth dataset in R to answer this question.



# Is the average length of tooth growth in guinea pigs different for
# guinea pigs receiving an Orange Juice supplement
# than it is for guinea pigs receiving a Vitamin C supplement?
?ToothGrowth
gpsupp.k <- kruskal.test(len ~ supp,data=ToothGrowth)
gpsupp.k
