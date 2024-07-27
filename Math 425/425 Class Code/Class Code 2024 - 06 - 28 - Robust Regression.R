plot(hp ~ qsec, data=mtcars, col=as.factor(cyl))

lm1 <- lm(hp ~ qsec + cyl + qsec:cyl, data=mtcars)
summary(lm1)

points(lm1$fit ~ qsec, data=mtcars, col=as.factor(cyl), pch=16, cex=0.5)

b <- coef(lm1)
b

drawit <- function(cyl=0, i=1){
  curve(b[1] + b[2]*qsec + b[3]*cyl + b[4]*qsec*cyl, 
        add=TRUE, xname="qsec", col=palette()[i])
}

drawit(cyl=4, i=1)
drawit(cyl=6, i=2)
drawit(cyl=8, i=3)






plot(hp ~ qsec, data=mtcars, col=as.factor(cyl))

lm2 <- lm(hp ~ qsec + as.factor(cyl) + qsec:as.factor(cyl), data=mtcars)
summary(lm2)

points(lm2$fit ~ qsec, data=mtcars, col=as.factor(cyl), pch=16, cex=0.5)

b <- coef(lm2)
b

drawit <- function(cyl6=0, cyl8=0, i=1){
  curve(b[1] + b[2]*qsec + b[3]*cyl6 + b[4]*cyl8 + b[5]*qsec*cyl6 + b[6]*qsec*cyl8, 
        add=TRUE, xname="qsec", col=palette()[i])
}

drawit(cyl6=0, cyl8=0, i=1)
drawit(cyl6=1, cyl8=0, i=2)
drawit(cyl6=0, cyl8=1, i=3)










library(dplyr)
View(starwars)
star.lm <- lm(mass ~ height + species, data=starwars)
summary(star.lm)

plot(mass ~ height, data=starwars, col=as.factor(species))

b <- coef(star.lm)
b

for (i in 3:length(b)) {
  abline(b[1] + b[i], b[2])
}

library(tidyverse)

ggplot(starwars, aes(x=height, y=mass)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~species)




library(car)
library(MASS)

plot(weight ~ repwt, data=Davis)

dav.lm <- lm(weight ~ repwt, data=Davis)
dav.rlm <- rlm(weight ~ repwt, data=Davis)

abline(dav.lm)
abline(dav.rlm, col="red")

summary(dav.lm)
summary(dav.rlm)



plot(dav.lm, which=c(1,4,5))

