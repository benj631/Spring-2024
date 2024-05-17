# Skills test Wk 1, lm


?airquality

mylm <- lm(Wind ~ Temp, data=airquality)
summary(mylm)

b <- coef(mylm)

plot(Wind ~ Temp, data=airquality)
abline(b[1] + b[2]*x)
abline(b[1], b[2], col = "red")

prediction <- predict(mylm, newdata = data.frame(Temp = 72))
prediction

par(mfrow=c(1,3))
plot(mylm,which=1:2)
plot(mylm$residuals)

?mtcars

cars_lm <- lm(mpg ~ wt, data=mtcars)
summary(cars_lm)


b <- coef(cars_lm)
plot(mpg ~ wt, data=mtcars)
abline(b[1], b[2], col = "red")

prediction <- predict(cars_lm, newdata = data.frame(wt = 3))
prediction

par(mfrow=c(1,3))
plot(cars_lm,which=1:2)
plot(cars_lm$residuals)

# Untrustworthy Slope and intercept

