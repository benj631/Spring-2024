set.seed(1)

n <- 100
x <- runif(n, 0, 10)


beta0 <- -6
beta1 <- 1.2


pi_i <- exp(beta0 + beta1*x)/(1 + exp(beta0 + beta1*x))
  

y <- rbinom(n, 1, pi_i)

mydata <- data.frame(y, x)

myglm <- glm(y ~ x, data=mydata, family="binomial")
summary(myglm)

b <- coef(myglm)

plot(y ~ x, data=mydata, pch=16, cex=2, col=rgb(0,0.8,0,.1))
curve(exp(b[1] + b[2]*x)/(1+exp(b[1] + b[2]*x)), add=TRUE, col="green3", lwd=2)
abline(h=0.5, v=5, lty=2, col="gray")
curve(exp(beta0 + beta1*x)/(1+exp(beta0 + beta1*x)), add=TRUE, col="green3", lty=2)
legend("topleft", bty='n', legend=c("Estimated","True"), lty=c(1,2), col="green3")
