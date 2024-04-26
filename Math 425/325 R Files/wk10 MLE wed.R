palette(c("skyblue","firebrick"))



# mpg ~ qsec + qsec:am
# ... ~ 
# am categorical division
# qsec:am is the interaction term.
# as.factor(tells r its categorical)


plot(mpg ~ qsec, data=mtcars, col=as.factor(am), xlim=c(0,30), ylim=c(-30,40), main="1974 Motor Trend Cars", pch=16)

lmcars <- lm(mpg ~ qsec + as.factor(am), data = mtcars)

summary(lmcars)

b = coef(lmcars)

abline(b[1], b[2], col=palette()[1])

abline( (b[1]+b[3]), (b[2]), col=palette()[2])

legend("topleft", legend=c("automatic","manual"), pch=1, col=palette(), title="Transmission (am)", bty="n")


# t value ran with Pr as p value.
# 

#Different slopes but same y intercept

palette(c("skyblue","firebrick"))

plot(mpg ~ qsec, data=mtcars, col=as.factor(am), xlim=c(0,30), ylim=c(-30,40), main="1974 Motor Trend Cars", pch=16)

lmcars <- lm(mpg ~ qsec + qsec:as.factor(am), data = mtcars)

summary(lmcars)

b = coef(lmcars)

abline(b[1], b[2], col=palette()[1])

abline( (b[1]), b[2]+b[3], col=palette()[2])

legend("topleft", legend=c("automatic","manual"), pch=1, col=palette(), title="Transmission (am)", bty="n")

# t test means red dots should have different slope than blue

palette(c("skyblue","firebrick"))

plot(mpg ~ qsec, data=mtcars, col=as.factor(am), xlim=c(0,30), ylim=c(-30,40), main="1974 Motor Trend Cars", pch=16)

lmcars <- lm(mpg ~ qsec + as.factor(am) + qsec:as.factor(am), data = mtcars)

summary(lmcars)

b = coef(lmcars)

abline(b[1], b[2], col=palette()[1])

abline( (b[1]+b[3]), b[2]+b[4], col=palette()[2])

legend("topleft", legend=c("automatic","manual"), pch=1, col=palette(), title="Transmission (am)", bty="n")

# we don't know if the data should have different y intercepts.