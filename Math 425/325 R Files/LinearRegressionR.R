library(mosaic)
library(tidyverse)
library(pander)
library(car)


plot(Height ~ Volume, data=trees)
trees.lm <- lm(Height ~ Volume, data=trees)
abline(trees.lm)

par(mfrow=c(1,2))
plot(trees.lm, which=1:2)
par(mfrow = c(1,1))

?faithful

faith <- lm(waiting ~ eruptions, data = faithful)
summary(faith)

# Extract the residuals
residuals <- residuals(faith)

# Plotting the scatterplot with the linear regression line
plot(faithful$waiting, faithful$eruptions, pch = 16, col = "blue", xlab = "waiting", ylab = "Y", main = "Scatterplot with Linear Regression Line")
abline(faith, col = "red")  # Adding the regression line using the 'faith' model

# Visualizing the residuals
plot(faithful$waiting, residuals, pch = 16, col = "green", xlab = "waiting", ylab = "Residuals", main = "Residual Plot")

par(mfrow=c(1,3))
plot(faith, which=1)
qqPlot(faith$residuals, id=FALSE)
plot(faith$residuals, main="Residuals vs Order")

predicted_waiting_time <- predict(faith, newdata = data.frame(eruptions = 3.5))

# Print the predicted waiting time
print(predicted_waiting_time)
