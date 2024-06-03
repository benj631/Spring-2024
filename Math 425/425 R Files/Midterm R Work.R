# Midterm Exam

library(mosaic)
library(tidyverse)
library(car)

View(airquality)
airlm <- lm(Ozone ~ Wind,data=airquality)
summary(airlm)

?RailTrail

rtlm <- lm(hightemp ~ lowtemp, data=RailTrail)
summary(rtlm)

View(RailTrail)

61 - (35.43521 + 0.72552 * 35)

SE = -6.6 * -6.6 + 13.8 * 13.8 + -9.8 * -9.8 + 4.6 * 4.6 + -2 ** -2

SE 

SE/3

?airquality

airlm2 <- lm(Wind~Temp, data=airquality)
summary(airlm2)


-.17 - -.167

t = -.003 / (3.142/ sqrt(153))

prediction = predict(airlm2, data.frame(Temp=0))
prediction

1.9845 + qt(c(.025,0.975),37)*.3521

#9
?mtcars

mtloglm <- lm(log(mpg) ~ wt, data=mtcars)
mtlm <- lm(mpg ~ wt, data=mtcars)
value <- exp(predict(mtloglm,data.frame(wt=2)))
=
plot(log(mpg)~wt,data=mtcars)

abline(mtlm)
abline(mtloglm)

value

#10

library(mosaic) #contains the Weather data
plot(high_temp ~ high_dewpt, data=Weather)
dplm <- lm(high_temp ~ high_dewpt, data=Weather)
abline(dplm)
summary(dplm)

value <- predict(dplm,data.frame(high_dewpt = 80))
value
91 + qt(c(.025,0.975),3653)* 0.477599

abline(h=90.06361, col="green")
abline(h=91.93639, col="green")


?abline

# 12

?KidsFeet
View(KidsFeet)

kfg <- filter(KidsFeet, sex == "G")
kfglm <- lm(width ~ length,data=kfg)
plot(width ~ length,data=kfg)
abline(kfglm)

value <- predict(kfglm, data.frame(length = 22))
value

summary(kfglm)
8.253979 + qt(c(.025,0.975),17) *(0.4)
abline(h=7.41, col="green")
abline(h=9.09, col="green")

# 14



plot(Ozone ~ Temp, data=airquality)
aqlm3 <- lm(Ozone ~ Temp, data=airquality)
aqlm3ss <- lm(sqrt(sqrt(Ozone)) ~ Temp, data=airquality)
aqlm3s <- lm(sqrt(Ozone) ~ Temp, data=airquality)
abline(aqlm3)
boxCox(aqlm3)
summary(aqlm3ss)
summary(aqlm3s)


#15

?ChickWeight

cwlm <- lm(weight ~ Time,data=ChickWeight)
plot(weight ~ Time,data=ChickWeight)
abline(cwlm)
plot(cwlm, which=1:3)
qqPlot(cwlm$residuals)






# 18


?mpg

View(mpg)

plot(hwy ~ cty, data = mpg)

mpg.lm <- lm(hwy ~ cty, data=mpg)
abline(mpg.lm)
summary(mpg.lm)


# 20

t = -109.736/374.404
2 * pt(-abs(t),28)


# 21

olm <- lm(circumference ~ age, data=Orange)
plot(olm,which=1:3)
plot(circumference ~ age, data=Orange)
abline(olm)


# 23 
data(cars)
View(cars)
clms <- lm(dist ~ speed, data=cars)
clms <- lm(sqrt(dist) ~ speed, data=cars)
clmss <- lm(sqrt(sqrt(dist)) ~ speed, data=cars)
clms <- lm(sqrt(dist) ~ speed, data=cars)
clmlog <- lm(log(dist) ~ speed, data=cars)
clm2 <- lm((dist)^2 ~ speed, data=cars)
boxCox(clm)


bs <- coef(clm)
bss <- coef(clm)
b2 <- coef(clm)


plot(dist ~ speed, data=cars)
abline (clm)
abline((clms))
abline(clmss)
abline(clmlog)
abline(clm2)

# 24
Loblolly

plot(height ~ age,data=Loblolly)
lbl.lm <- lm(height ~ age, data=Loblolly)
abline(lbl.lm)

plot(lbl.lm, which= 1:3)
