# Wk 5 Notes 425

library(tidyverse)

faith.lm <- lm(waiting ~ eruptions, data=faithful)

ggplot(faithful, aes(x=eruptions, y = waiting))+
  geom_point() +
  geom_smooth(method ="lm", se=T, forumla = y~x) +
  geom_segment(x=2,xend=2, y=43.2,yend=66.2) +
  theme_bw

predict(faith.lm, data.frame(eruptions = 2))
predict(faith.lm, data.frame(eruptions = 2), interval = "prediction")
predict(faith.lm, data.frame(eruptions = 2), interval = "confidence")

#confidence is not a prediction

# Tranformations

u.lm.t <- lm(sqrt(sqrt(gasbill)) ~ temp, data=Utilities)

Mypreds <- predict(ul.lm.t, data.frame(temp=30), interval="prediction"^4
                   Mypreds2 <- predict(ul.lm.t, data.frame(temp=60), interval="prediction"^4
                                       
                                       Mypreds <- predict(u.lm.t, data.frame(temp=30), interval="prediction")^4
                                       
                                       Plot(gasbill ~ temp, data=utilities)
                                       B <- coef(u.lm.t)
                                       Curve(b[1] + b[2]*x^4, add=TRUE)
                                       Lines(c(30,30), mypreds[2:3], lwd = 3, col=rgb(.53, 0.81, .92, .5)
                                       Lines(c(60,60), mypreds2[2:3], lwd = 3, col=rgb(.53, 0.81, .92, .5)
  