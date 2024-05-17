# In Class logistic Regression

library(ggplot2)
library(mosaic)
library(dplyr)

View(KidsFeet)

kf <- KidsFeet %>%
  mutate(sex_binary = case_when(
    sex == "B" ~ 1,
    sex == "G" ~ 0
  ))

kf_glm <- glm(sex_binary ~ length, data = kf, family = binomial)
summary(kf_glm)


palette(c("lightblue","pink"))

b = coef(kf_glm)

print(b[1])
print(b[2])

# Plotting a scatter plot
plot(kf$length,kf$sex_binary, xlab = "Length", ylab = "Sex", main = "Scatter plot with Logistic Regression Curve")
curve(exp(b[1] + b[2]*x) / (1 + exp(b[1] + b[2]*x)), add = TRUE, col = "blue")

predicted_probabilities <- predict(kf_glm, newdata = data.frame(length = x_values), type = "response")

x_values <- seq(min(KidsFeet$length), max(KidsFeet$length), length.out = 100)
  lines(x_values, predicted_probabilities, col = "blue", lwd = 2)


predict(kf_glm, newdata=data.frame(length = 25), type="response")

ggplot(data=KidsFeet, aes(x=length,y=sex)) +
  geom_point() +
  geom_smooth(method="glm",method.args = list(family="binomial"), se = FALSE) +
  theme_bw()

curve(exp(b[1] + b[2]*x) / (1 + exp(b[1] + b[2]*x)), add = TRUE, col = "blue")


print(exp(b[2]))
