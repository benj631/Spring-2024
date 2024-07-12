library(tidyverse)


cars.lm <- lm(dist ~ speed, data=cars)
cars.lm$fitted.values
summary(cars.lm)

ggplot(cars, aes(x=speed, y=dist)) + 
  geom_point() + 
  geom_smooth(method="lm", se=F, formula=y~x, aes(linetype="Regression Line")) + 
  geom_hline(aes(yintercept=mean(dist), linetype="Y-bar")) +
  labs(title="main title", x="speed of cars", y="distance in feet", linetype="bob") +
  geom_segment(aes(x=speed, xend=speed, y=dist, yend=cars.lm$fit)) + 
  geom_rect(aes(xmin=speed, xmax=speed-cars.lm$res*0.15, ymin=dist, ymax=cars.lm$fit), alpha=0.1)


