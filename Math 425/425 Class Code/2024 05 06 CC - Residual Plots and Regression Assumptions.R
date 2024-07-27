plot(height ~ age, data=Loblolly)
lob.lm <- lm(height ~ age, data=Loblolly)
abline(lob.lm)
plot(lob.lm, which=1)
