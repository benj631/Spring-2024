# Base Graphic

plot(circumference ~ age, data=Orange, pch=16, col="red", main="Growth of Orange Trees", xlab="Age of Tree in Days", ylab="Circumference of Tree (mm)")

# ggplot Graphic
library(tidyverse)

ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="red") +
  labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)") + 
  theme_bw( )



lm.log <- lm(log(circumference) ~ age, data=Orange)
lm.sqrt <- lm(sqrt(circumference) ~ age, data=Orange)
lm.1oy <- lm(1/(circumference) ~ age, data=Orange)
lm.y <- lm(circumference ~ age, data=Orange)
lm.y2 <- lm(circumference^2 ~ age, data=Orange)
lm.ss <- lm(sqrt(sqrt(circumference)) ~ age, data=Orange)

library(car)
boxCox(lm.y)


b.log <- coef(lm.log)
b.sqrt <- coef(lm.sqrt)
b.1oy <- coef(lm.1oy)
b <- coef(lm.y)
b2 <- coef(lm.y2)
bss <- coef(lm.ss)



plot(circumference ~ age, data=Orange, pch=16, col="red", main="Growth of Orange Trees", xlab="Age of Tree in Days", ylab="Circumference of Tree (mm)")
curve(exp(b.log[1] + b.log[2]*x), add=TRUE, col="pink")
curve((b.sqrt[1] + b.sqrt[2]*x)^2, add=TRUE, col="green")
curve(1/(b.1oy[1] + b.1oy[2]*x), add=TRUE, col="orange")
curve((b[1] + b[2]*x), add=TRUE, col="blue")
curve(sqrt(b2[1] + b2[2]*x), add=TRUE, col="purple")
curve((bss[1] + bss[2]*x)^4, add=TRUE, col="darkgreen")

legend("topleft", legend=c("log","sqrt","1/y","y","y2","sqrt(sqrt(y))"), 
       lty=1, col=c("pink","green","orange","blue","purple","darkgreen"))



ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="red") +
  labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)") + 
  stat_function(fun=function(x) exp(b.log[1] + b.log[2]*x), aes(color="log")) + 
  stat_function(fun=function(x) (b.sqrt[1] + b.sqrt[2]*x)^2, aes(color="sqrt")) + 
  stat_function(fun=function(x) 1/(b.1oy[1] + b.1oy[2]*x), aes(color="1oy")) + 
  stat_function(fun=function(x) b[1] + b[2]*x, aes(color="y")) + 
  stat_function(fun=function(x) sqrt(b2[1] + b2[2]*x), aes(color="y2")) + 
  stat_function(fun=function(x) (bss[1] + bss[2]*x)^4, aes(color="sqrt(sqrt(y))")) + 
  theme_bw( ) + 
  ylim(c(0,250))



plot(lm.log, which=1)
plot(lm.sqrt, which=1)
plot(lm.1oy, which=1)
plot(lm.y, which=1)
plot(lm.y2, which=1)



YoungOrange <- Orange %>%
  filter(age < 1200)


lm.log <- lm(log(circumference) ~ age, data=YoungOrange)
lm.sqrt <- lm(sqrt(circumference) ~ age, data=YoungOrange)
lm.1oy <- lm(1/(circumference) ~ age, data=YoungOrange)
lm.y <- lm(circumference ~ age, data=YoungOrange)
lm.y2 <- lm(circumference^2 ~ age, data=YoungOrange)
lm.ss <- lm(sqrt(sqrt(circumference)) ~ age, data=YoungOrange)


b.log <- coef(lm.log)
b.sqrt <- coef(lm.sqrt)
b.1oy <- coef(lm.1oy)
b <- coef(lm.y)
b2 <- coef(lm.y2)
bss <- coef(lm.ss)

ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="orangered") +
  labs(title="Growth of Orange Trees", x="Age of Tree in Days", y="Circumference of Tree (mm)") + 
  stat_function(fun=function(x) exp(b.log[1] + b.log[2]*x), aes(color="log")) + 
  stat_function(fun=function(x) (b.sqrt[1] + b.sqrt[2]*x)^2, aes(color="sqrt")) + 
  stat_function(fun=function(x) 1/(b.1oy[1] + b.1oy[2]*x), aes(color="1oy")) + 
  stat_function(fun=function(x) b[1] + b[2]*x, aes(color="y")) + 
  stat_function(fun=function(x) sqrt(b2[1] + b2[2]*x), aes(color="y2")) + 
  stat_function(fun=function(x) (bss[1] + bss[2]*x)^4, aes(color="sqrt(sqrt(y))")) + 
  theme_bw( ) + 
  ylim(c(0,250))