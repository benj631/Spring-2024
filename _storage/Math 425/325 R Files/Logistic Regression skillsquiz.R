# Logistic Regression Mobius

View(infert)
?infert
infert.glm <- glm( (spontaneous > 0) ~ age, data=infert, family=binomial)

summary(infert.glm)
plot( (spontaneous > 0) ~ age, data=infert)
b <- coef(infert.glm)
print(b[1])
print(b[2])
# curve( exp(b0 + b1*x)/(1 + exp(b0 + b1*x)), add=TRUE)
curve(exp(b[1] + b[2]*x)/(1 + exp(b[1] + b[2]*x)), add=TRUE)

table(infert$age)
# Goodness of fit test
# residual deviance and df are found at the bottom of summary()
# pchisq(residual deviance, df for residual deviance, lower.tail=FALSE)
pchisq(334.01, 246, lower.tail=FALSE)

# ->  0.0001571274 , logistic regression not useful for the data


library(mosaic)
View(Galton)
?Galton

Galton <- Galton %>%
  mutate(
    isMale = case_when(
      sex == "M" ~ 1,
      sex == "F" ~ 0,
      TRUE ~ NA_integer_  # Default case (if any)
    )
  )

galt.glm <- glm((isMale > 0) ~ height, data=Galton, family=binomial)
summary(galt.glm)
plot( (isMale > 0) ~ height, data=Galton)
b <- coef(galt.glm)
print(b[1])
print(b[2])

curve(exp(b[1] + b[2]*x)/(1 + exp(b[1] + b[2]*x)), add=TRUE)

table(Galton$isMale)

print(exp(b[2]))

print(predict(galt.glm, newdata = data.frame(height = 70), type = "response"))
print(predict(galt.glm, newdata = data.frame(height = 65), type = "response"))

install.packages("ResourceSelection")
library(ResourceSelection)
# Actually y
hoslem.test(galt.glm$y,galt.glm$fitted, g=10)
      