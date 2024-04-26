# Multiple Linear Regression
library(plotly)
library(dplyr)
library(mosaic)
?SaratogaHouses
View(SaratogaHouses)

install.packages("manhattanly")
library(manhattanly)

SH2 <- filter(SaratogaHouses, bedrooms == 3,  newConstruction=="Yes")
View(SH2)

sh2.lm <- lm(price ~ livingArea + fireplaces + livingArea:fireplaces, data= SH2)
summary(sh2.lm)


b <- coef(sh2.lm)

plot(SH2$livingArea, SH2$price, xlab = "Living Area", ylab = "Price")

# Overlaying first linear regression line
abline(b[1], b[2], col = "red")

# Overlaying second linear regression line
abline(b[1] + b[3], b[2] + b[4], col = "blue")


library(car)


sh2.res <- residuals(sh2.lm)

par(mfrow=c(1,3))
plot(sh2.lm, which=1)
qqPlot(sh2.lm$residuals, id=FALSE)
plot(sh2.res)
# sh2$residuals


?airquality
View(airquality)

# I is a function to protect the squared variable to properly work with the lm
air.lm.quad <- lm(Temp ~ Month + I(Month^2), data=airquality)
summary(air.lm.quad)

par(mfrow=c(1,3))
plot(air.lm.quad, which=1)
qqPlot(air.lm.quad, id=FALSE)
plot(air.lm.quad$residuals)

# 126 and 127 answers for mobius

# Create a QQ plot
qqnorm(air.lm.quad$residuals)
qqline(air.lm.quad$residuals)

# Add labels to specific points (for example, every 10th point)
labels <- seq_along(air.lm.quad$residuals)
text(qqnorm(air.lm.quad$residuals)$x, qqnorm(air.lm.quad$residuals)$y, labels, pos = 3, offset = 0.5)

predict(air.lm.quad, data.frame(Month=7.129032))

plot(airquality$Month, airquality$Temp)

b <- coef(air.lm.quad)

curve(b[1]+b[2]*x + b[3]*x^2, add=TRUE, col="skyblue2", lwd=2)

lines(c(7.129032, 7.129032), c(0, 84.74588), lty=2, col="firebrick")

lines(c(0, 7.129032), c(84.74588, 84.74588), lty=2, col="firebrick")

points(7.129032, 84.74588, cex=1.2, col="red")

text(7.5, 85, "Predicted Temp", cex=0.5, pos=3)

#Find vertex
# ax^2 + bx + c 
# -3.28x^2 + 48.71 + -95.72 
# x co: -b/2a, y co -> plug in x co into the original equation

a <- -3.2828
b <- 48.7183

# Calculate x_v
x_v <- -b / (2 * a)

# Calculate y_v
y_v <- a * x_v^2 + b * x_v - 95.7256

# Display the vertex
cat("Vertex (Month, Temp): ", x_v, ",", y_v)

?RailTrail
View(RailTrail)

RailTrail <- mutate(RailTrail, rain = ifelse(precip > 0, 1, 0))

palette(c("orange","skyblue"))

plot(volume ~ hightemp, data=RailTrail, col=as.factor(rain), main="RailTrail Data Set")

legend("topleft", legend=c("No Rain", "Rain"), col=palette(), pch=16)

rain.lm <- lm(volume ~ hightemp + rain + hightemp:as.factor(rain), data = RailTrail)
summary(rain.lm)

rain.lm.noint <- lm(volume ~ hightemp + rain, data = RailTrail)
summary(rain.lm.noint)

b <- coef(rain.lm.noint)

curve(b[1] + b[2]*x, add = TRUE, col = "orange")
curve(b[1] + b[3] + b[2]*x, add = TRUE, col = "skyblue")

b[1] + b[3]

qqPlot(rain.lm.noint$residuals)

par(mfrow=c(1,3))
plot(rain.lm.noint, which=1)
qqPlot(rain.lm.noint, id=FALSE)
plot(rain.lm.noint$residuals)

