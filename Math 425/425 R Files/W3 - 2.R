# Wk 3 425

plot(circumference ~ age, data=Orange)
o.lm <- lm(circumference ~ age, data= Orange)
abline(o.lm)
plot(o.lm,which=1)
plot(o.lm,$residuals)
plot(o.lm$residuals, col = as.factor(Orange$Tree))
plot(o.lm$residuals, col = as.factor(Orange$Tree))



boxplot(mass ~ gender, data=starwars)
boxplot(log(mass) ~ gender, data=starwars)

View(islands)

hist(islands,col="forestgreen")
hist(log(islands),col="forestgreen")

  log(5000)

log(5000)

exp(4)
log(10000)


library(mosaicData)
View(Utilities)
?Utilities

plot(gasbill ~ temp, data=Utilities)
u.lm <- lm(gasbill ~ temp, data= Utilities)

plot(log(gasbill) ~ temp, data=Utilities)

u.lm.t <- lm(log(gasbill) ~ temp, data= Utilities)
abline(u.lm.t)
plot(u.lm.t)
summary(u.lm.t)

library(car)
boxCox(u.lm)

# .3668 comes from the residual in the log space

b <- coef(u.lm.t)
plot(gasbill~ temp, data=Utilities)
curve(exp(b[1] + b[2]*x), add=TRUE)
curve(exp(b[1]-.3668 + b[2]*x), add=TRUE)
curve(exp(b[1]+.3668 + b[2]*x), add=TRUE)

# Friday

library(tidyverse)
library(car)
?Orange

lm.y <- lm(circumference ~ age, data=Orange)

# Log is interpretable, loses 10% per x
# So if it works, you can use it.
# Sqrt is not very interpretable without derivatives

# Log likelihood of good fit
boxCox(lm.y)

lm.log <- lm(log(circumference) ~ age, data=Orange)
lm.sqrt <- lm(sqrt(circumference) ~ age, data=Orange)
lm.1oy <- lm(1/(circumference) ~ age, data=Orange)
lm.y2 <- lm(circumference^2 ~ age, data=Orange)
lm.ss <- lm(sqrt(sqrt(circumference)) ~ age, data=Orange)

b <- coef (lm.y)
b.log <- coef(lm.log)
b.sqrt <- coef(lm.sqrt)
b.1oy <- coef(lm.1oy)
b2 <- coef(lm.y2)
bss <- coef(lm.ss)

?pch

plot(circumference ~ age, data = Orange, pch=1,col="black")
curve(exp(b.log[1] + b.log[2]*x), add = TRUE,col="pink")
curve((b.sqrt[1] + b.sqrt[2]*x)^2, add = TRUE,col="green")
curve(1/(b.1oy[1] + b.1oy[2]*x), add = TRUE,col="orange")
curve(sqrt(b2[1] + b2[2]*x), add = TRUE,col="blue")
curve(exp(b.log[1] + b.log[2]*x), add = TRUE,col="steelblue")
curve((bss[1] + bss[2]*x)^4, add = TRUE,col="purple")

legend("topleft", legend=c("y", "log", "sqrt","1/y","b2","")

# Base Graphic

plot(circumference ~ age, data=Orange, pch=16, col="orangered", main="Growth of Orange Trees", xlab="Age of Tree in Days", ylab="Circumference of Tree (mm)")

# ggplot Graphic

ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="orangered") +
  labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)") + 
  stat_function(fun=function(x) exp(b.log[1] + b.log[2]*x)) + 
  stat_function(fun=function(x) (b.sqrt[1] + b.sqrt[2]*x)^2)) + 
  stat_function(fun=function(x) exp(b.log[1] + b.log[2]*x)) + 
  stat_function(fun=function(x) exp(b.log[1] + b.log[2]*x)) + 
  stat_function(fun=function(x) exp(b.log[1] + b.log[2]*x)) + 
  stat_function(fun=function(x) exp(b.log[1] + b.log[2]*x)) + 
  stat_function(fun=function(x) exp(b.log[1] + b.log[2]*x)) + 
  theme_bw( )

plot(lm.log, which=1)


# 3 Equations: y prime, (get extremes), prime should be some transformation
# Y'i = Sqrt(Yi)
# State Y hat without a prime - back in the real world

# X transformations are an art, not a science
# You guess, and sometimes it works


