library(mosaic)
library(tidyverse)
library(car)


plot(gasbill ~ temp, data=Utilities)
u.lm <- lm(gasbill ~ temp, data=Utilities)

boxCox(u.lm)

u.lm.t <- lm(sqrt(sqrt(gasbill)) ~ temp, data=Utilities)

mypreds <- predict(u.lm.t, data.frame(temp=30), interval="prediction")^4
mypreds2 <- predict(u.lm.t, data.frame(temp=60), interval="prediction")^4

plot(gasbill ~ temp, data=Utilities)
b <- coef(u.lm.t)
curve((b[1] + b[2]*x)^4, add=TRUE)
lines(c(30,30), mypreds[2:3], lwd=3, col=rgb(.53, 0.81, 0.92, 0.5))
lines(c(60,60), mypreds2[2:3], lwd=3, col=rgb(.53, 0.81, 0.92, 0.5))


u.lm <- lm(gasbill ~ temp, data=Utilities)

mypreds <- predict(u.lm, data.frame(temp=30), interval='prediction')
mypreds2 <- predict(u.lm, data.frame(temp=60), interval='prediction')
plot(gasbill ~ temp, data=Utilities)
abline(u.lm)
lines(c(30,30), mypreds[2:3], lwd=3, col=rgb(.53, 0.81, 0.92, 0.5))
lines(c(60,60), mypreds2[2:3], lwd=3, col=rgb(.53, 0.81, 0.92, 0.5))




mypreds <- predict(u.lm.t, data.frame(temp=30), interval="prediction")^4
mypreds2 <- predict(u.lm.t, data.frame(temp=60), interval="prediction")^4

temps <- seq(10, 80, 5)
allpreds <- predict(u.lm.t, data.frame(temp=temps), interval="prediction")^4
allpreds <- data.frame(allpreds, temp=temps)


ggplot(Utilities, aes(x=temp, y=gasbill)) + 
  geom_point() + 
  geom_smooth(method="lm", se=T, formula=y~x) + 
  geom_segment(aes(x=30, xend=30, y=mypreds[2], yend=mypreds[3]), 
               lwd=3, color="skyblue", alpha=0.01) + 
  geom_line(data=allpreds, mapping=aes(x=temp, y=lwr)) + 
  geom_line(data=allpreds, mapping=aes(x=temp, y=upr))



