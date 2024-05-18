# WK 3 - 1 Residual Plots

View(Loblolly)
plot(height ~ age, data = Loblolly)
lob.lm <- lm(height ~ age, data= Loblolly)
prediction <- predict(lob.lm, newdata = data.frame(age = 40))
prediction
abline(lob.lm)
plot(lob.lm, which=1)

which(names(lob.lm$fit)==83)
