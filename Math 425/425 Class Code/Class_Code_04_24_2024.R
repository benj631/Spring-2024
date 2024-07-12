



X <- rnorm(153, 12, 3)
beta0 <- 89
beta1 <- -1.25
sigma <- 8

Y <- beta0 + beta1*X + rnorm(153, 0, 8)  

plot(Y ~ X)  
mylm <- lm(Y ~ X)
summary(mylm)



library(tidyverse)

plot(Temp ~ Day, data=filter(airquality, Month==5))
mylm <- lm(Temp ~ Day, data=filter(airquality, Month==5))
abline(mylm)
summary(mylm)