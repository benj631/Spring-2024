# Week 2 Mobius - Residuals

library(tidyverse)
library(dplyr)
library(ggplot)
library(pander)

View(Orange)


mylm <- lm(circumference ~ age, data=Orange)
summary(calm)

# b0/intercept, b1/slope

y <- 17.39965 + 0.106770

y <- 17.39965 + 0.106770 * 3 * 365
y

plot(circumference ~ age, data=Orange,col="orangered",pch=16)
abline(calm,col="blue")

# Y is the actual Y value of the data set - circumference
sum((Y - mylm$fit)^2 )
sum((Orange$circumference - mylm$fit)^2 )

sum((mylm$fit - mean(Y))^2 )
sum((mylm$fit - mean(Orange$circumference))^2 )

sum( (Y - mean(Y))^2 )
sum((Orange$circumference - mean(Y))^2 )

# Correlation
cor(Orange$circumference,Orange$age)

clm1 <- lm(mpg ~ wt, data = mtcars)
clm2 <- lm(mpg ~ cyl, data = mtcars)
clm3 <- lm(mpg ~ hp, data = mtcars)

# Set up the plotting area to have 1 row and 3 columns
par(mfrow = c(2, 3))

# Plot mpg vs wt with regression line
plot(mpg ~ wt, data = mtcars, col = "red", main = "MPG vs Weight", xlab = "Weight (1000 lbs)", ylab = "MPG")
abline(clm1, col = "pink")

# Plot mpg vs cyl with regression line
plot(mpg ~ cyl, data = mtcars, col = "blue", main = "MPG vs Cylinders", xlab = "Cylinders", ylab = "MPG")
abline(clm2, col = "lightblue")

# Plot mpg vs hp with regression line
plot(mpg ~ hp, data = mtcars, col = "seagreen", main = "MPG vs Horsepower", xlab = "Horsepower", ylab = "MPG")
abline(clm3, col = "green")

summary(clm1)
summary(clm2)
summary(clm3)


library(pander)
install.packages("gridExtra")
library(gridExtra)
library(ggplot2)

# Load the mtcars dataset
data(mtcars)


# Correlation
cor(Orange$circumference,Orange$age)


# Create linear models
clm1 <- lm(mpg ~ wt, data = mtcars)
clm2 <- lm(mpg ~ cyl, data = mtcars)
clm3 <- lm(mpg ~ hp, data = mtcars)


plot(clm1, which = 1:3)
plot(clm2, which = 1:3)
plot(clm3, which = 1:3)


sum((mtcars$mpg - clm1$fit)^2 )
sum((clm1$fit - mean(mtcars$mpg))^2 )
sum((mtcars$mpg - mean(mtcars$mpg))^2 )

sum((mtcars$mpg - clm2$fit)^2 )
sum((clm2$fit - mean(mtcars$mpg))^2 )
sum((mtcars$mpg - mean(mtcars$mpg))^2 )

sum((mtcars$mpg - clm3$fit)^2 )
sum((clm3$fit - mean(mtcars$mpg))^2 )
sum((mtcars$mpg - mean(mtcars$mpg))^2 )




# Create the summary statistics using pander
summary_clm1 <- pander_return(summary(clm1))
summary_clm2 <- pander_return(summary(clm2))
summary_clm3 <- pander_return(summary(clm3))

# Create the plots using ggplot2
plot1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "pink") +
  ggtitle("MPG vs Weight") +
  xlab("Weight (1000 lbs)") +
  ylab("MPG") + 
  theme_bw()

plot2 <- ggplot(mtcars, aes(x = cyl, y = mpg)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "lightblue") +
  ggtitle("MPG vs Cylinders") +
  xlab("Cylinders") +
  ylab("MPG") + 
  theme_bw()

plot3 <- ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "seagreen") +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  ggtitle("MPG vs Horsepower") +
  xlab("Horsepower") +
  ylab("MPG") + 
  theme_bw()

# Combine the plots and summaries using gridExtra
grid.arrange(
  plot1, plot2, plot3,
  tableGrob(summary_clm1),
  tableGrob(summary_clm2),
  tableGrob(summary_clm3),
  ncol = 3
)

