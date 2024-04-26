# Class Activity 425 W1

View(airquality)
?airquality

library(dplyr)
library(mosaic)


airquality = airquality %>% mutate(
  tempRange = case_when(
    airquality.temp < 60 ~ 55
    61 < airquality.temp < 70 ~ 65
    71 < airquality.temp < 80 ~ 75
    81 < airquality.temp < 90 ~ 85
    91 < airquality.temp < 100 ~ 95
    
  )
)


hist(airquality$Temp)
# Hist for Frequency - 1 numeric variable
# Barplot - Count for categorical

mean(airquality$Temp)
sd(airquality$Temp)

boxplot(airquality$Temp ~ as.factor(airquality$Month))


# Try Mosaic Fabstats

favstats(Temp ~ Month, data=airquality)

# How can reduce variability for prediction?


ggplot(airquality, aes(x=Temp,y=Wind)) + 
  geom_point()+
  geom_smooth(method = "lm", formula = y~x)

mylm <- lm(Temp ~ Wind, data=airquality)
summary(mylm)

b <- coef(mylm)

b[1]
b[2]
b[1] + b[2]*19
