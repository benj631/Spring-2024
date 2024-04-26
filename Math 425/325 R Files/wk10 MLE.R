library(tidyverse)
library(mosaic)
library(car)
library(pander)

?mtcars


#cars.lm
cars.lm <- lm(mpg ~ qsec + am + qsec:am, data = mtcars)
summary(cars.lm)


par(mfrow=c(1,1))
    

palette(c("blue","red"))

plot(mpg ~ qsec, data = mtcars, col=as.factor(am), pch = 16, xlim=c(-5,30), ylim=c(-25,40))

# the xlim and y lim allow us to see the y intercepts

legend("topleft", col=palette(), pch = 21,
       title ="Transmisison (am)",
       title.col="black",
       legend = c("Automatic", "Manual"),
       bty="n",
       text.col = palette())

# We want to see mpg broken up for each set, not the total?...

# b stood for the coefficients of our model (guess curve)
# b will store those coefficients.
b <- coef(cars.lm)

b[1]

#*x because it's literally applying it to the x coordinate
#  R starts at index 1, b corresponds with the model terms.
# See the cars.lm object to see the coefficients.

curve(b[1] + b[2]*x, col="blue", add=TRUE)
curve((b[1]+b[3]) +(b[2] + b[4])*x, col="red", add=TRUE)

# Automatic y intercept
print(b[1])

# Automatic slope
print(b[2])

# Manual y intercept
print(b[1]+b[3])

# Manual slope
print(b[2]+ b[4])

View(cars.lm)

#bty is a box around the legend, n means no

?curve

plot(cars.lm)

pander(summary(lm.2lines)$coefficients)
