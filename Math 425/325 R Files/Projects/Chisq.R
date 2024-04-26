library(pander)
library(dplyr)
library(car)

glasses <- cbind( Males = c(Glasses = 5, Contacts = 12, None = 18), Females = c(Glasses = 4, Contacts = 14, None = 22))

glasses

barplot(glasses, beside=TRUE, legend.text=TRUE, args.legend=list(x = "topleft", bty="n"))

chis.glasses <- chisq.test(glasses)

chis.glasses$expected

summary(chis.glasses)

chis.glasses



education <- cbind( `United States` = c(Engineering = 61941, `Natural Science` = 111158, `Social Science` = 182166), `Western Europe` = c(Engineering = 158931, `Natural Science` = 140126, `Social Science` = 116353), Asia = c(280772, 242879, 236018))

education

chis.education <- chisq.test(education)
chis.education$expected

barplot(education, beside=TRUE, legend.text=TRUE, args.legend=list(x="topleft", bty="n"))

residuals(chis.education)

barplot(residuals(chis.education))



?InsectSprays

View(InsectSprays)


summed_data <- X %>%
  group_by(spray) %>%
  summarise(total_count = sum(count))


X <- table(InsectSprays$count, InsectSprays$spray)

X

xaov <- aov(count ~ spray, data = InsectSprays)
summary(xaov)

y <- residuals(xaov)

plot(y)
qqPlot(xaov)
