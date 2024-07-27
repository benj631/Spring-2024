library(mosaic)
library(tidyverse)


pairs(mtcars, panel=panel.smooth)

mt2 <- mtcars %>%
  mutate(y = ifelse(mpg > 20, 1, 0))

pairs(mt2, panel=panel.smooth, col=rgb(.1,.1,.1, .1), pch=16)



glm.y <- glm(y ~ qsec, 
                data=mt2, family=binomial)
summary(glm.y)

glm.cube <- glm(y ~ qsec + I(qsec^2) + I(qsec^3), 
                data=mt2, family=binomial)
summary(glm.cube)

plot(y ~ qsec, data=mt2, col=rgb(.1,.1,.1, .1), pch=16)
b <- coef(glm.cube)
curve(exp(b[1] + b[2]*x + b[3]*x^2 + b[4]*x^3)/(1+exp(b[1] + b[2]*x + b[3]*x^2 + b[4]*x^3)), add=TRUE)




glm.quad <- glm(y ~ qsec + I(qsec^2), 
                data=mt2, family=binomial)
summary(glm.quad)

plot(y ~ qsec, data=mt2, col=rgb(.1,.1,.1, .1), pch=16, xlim=c(-50,24))
b <- coef(glm.quad)
curve(exp(b[1] + b[2]*x + b[3]*x^2)/(1+exp(b[1] + b[2]*x + b[3]*x^2)), add=TRUE)





mt3 <- mtcars %>%
  mutate(y = ifelse(hp > 123, 1, 0))

glm.quad <- glm(y ~ gear + I(gear^2), 
                data=mt3, family=binomial)
summary(glm.quad)

plot(y ~ gear, data=mt3, col=rgb(.1,.1,.1, .1), pch=16)
b <- coef(glm.quad)
curve(exp(b[1] + b[2]*x + b[3]*x^2)/(1+exp(b[1] + b[2]*x + b[3]*x^2)), add=TRUE)





plot(gasbill>100 ~ month, data=Utilities, pch=16, col=rgb(.1,.1,.1,.1))



plot(mpg ~ qsec, data=mtcars, col=as.factor(am), pch=16)





glm.2l <- glm(y ~ qsec + am, 
                data=mt2, family=binomial)
summary(glm.2l)

palette(c("skyblue","orange"))
plot(y+rnorm(32,0,0.01) ~ qsec, data=mt2, col=as.factor(am), pch=16)
b <- coef(glm.2l)
x2=0
curve(exp(b[1] + b[2]*x + b[3]*x2)/(1+exp(b[1] + b[2]*x + b[3]*x2)), add=TRUE, col=palette()[1])
x2=1
curve(exp(b[1] + b[2]*x + b[3]*x2)/(1+exp(b[1] + b[2]*x + b[3]*x2)), add=TRUE, col=palette()[2])


exp(-567.304)




glm.2l <- glm(y ~ qsec + am + cyl, 
              data=mt2, family=binomial)
summary(glm.2l)



glm(heartattack ~ gender + ethnicity + income + occupat + ldl + hdl + height + weight, )

