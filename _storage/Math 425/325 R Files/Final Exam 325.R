# Final Exam

library(car)
library(mosaic)
library(tidyverse)
library(dplyr)


# 1

?Marriage
View(Marriage)
Marriage2 <- subset(Marriage, Marriage$person == "Bride" )
View(Marriage2)
mean(Marriage2$age)

# 2 Predict av volume

?RailTrail

cloud <- lm(volume ~ cloudcover, data=RailTrail)
predict(cloud,data.frame(cloudcover = 8))

# 3 How many times did it rain on a weekday

View(RailTrail)
weekdayrt <- subset(RailTrail, RailTrail$dayType == "weekday")
weekdayrtrain <- subset(weekdayrt, weekdayrt$precip > 0.00)
view(weekdayrt)

length(weekdayrtrain)

# 4

View(Marriage)

Marriage <- Marriage %>%
  mutate(
    previouslyMarried = case_when(
      (Marriage$prevcount > 0) ~ 1,
      (Marriage$prevcount == 0) ~ 0
    )
  )

prevMar <- glm(previouslyMarried ~ age, data=Marriage)
plot(Marriage$age, Marriage$previouslyMarried)
predict(prevMar, data.frame(age=27))

# 5

?Salaries

wilcox.test(discipline ~ as.factor(rank),data=Salaries)


# 5
?TenMileRace

plot(time ~ age, data=TenMileRace, col=sex)

TenMileRace2 <- TenMileRace %>%
  mutate(
    sexBin = case_when(
      (sex == "F") ~ 1,
      (sex == "M") ~ 0
    )
  )

tenmilesex <- lm(time ~ age + sexBin, data = TenMileRace2)
b <- coef(tenmilesex)

b[1]
b[1] + b[3]
b[1] - (b[1] + b[3])


# 10

?InsectSprays
View(InsectSprays)

kruskal.test(count ~ spray, data=InsectSprays)


# 12

View(TitanicSurvival)
x <- table(TitanicSurvival$sex,TitanicSurvival$survived)
chisq.test(x)

#14

fmsv <- filter(TitanicSurvival, TitanicSurvival$sex == "female")
length(fmsv$sex)

m <- -.1926

x <- m*40000 + .1202*40000 + 19408 - 2002
x

#17

?starwars
View(starwars)
sw.humans <- filter(starwars,species == "Human")

sw.humans <- sw.humans %>%
  mutate(
    sex = case_when(
      gender == "masculine" ~ 1,
      gender == "feminine" ~ 0
    )
  )
sw.humans = na.omit(sw.humans)

wilcox.test(species ~ sex, data=sw.humans, mu=0, alternative= "two.sided")


# 21

?Davis
View(Davis)
wilcox.test(Davis$weight, Davis$repwt, mu=0)
