# Wk 7-2 

library(mosaic)
library(tidyverse)

lm.quad <- lm(gasbill ~ month + I(month^2), data=Utilities)
b <- coef(lm.quad)

plot(gasbill ~ month, data = Utilities)
curve(b[1]+ b[2]*x + b[3]*x^2, add = TRUE, col="skyblue", lwd = 2)
lines(lowess(Utilities$month, Utilities$gasbill), col='firebrick', lwd=2)

ggplot(Utilities, aes(x=month,y=gasbill)) +
  geom_point() +
  geom_smooth(color = 'firebrick') +
  # Poly function increases the exponent of the model
  # Overfitting as you increase exponentiation
  geom_smooth(method='lm', formula=y~poly(x,2), color="skyblue", se=F) +
  theme_bw()
