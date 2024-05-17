#Replace lets numbers go back into the pool
# Same thing -> Different results -> statistics (insanity)


X <- sample(1:21, 1000, replace = TRUE)
beta0 <- 89
beta1 <- -1.25
sigma <- 8

#rnorm creates a normal distribution

Y <- beta0 + beta1*X + rnorm(153,0,8)

plot(Y ~ X)

mylm <- lm(Y ~ X)
summary(mylm)

b <- coef(mylm)
curve(b[1] + b[2]*x, add=TRUE)


# confidence intervals -> overlaps -> agreement
# All models are wrong, some are useful
# A bad model -> ybar (mean) sometimes the mean is a good guess