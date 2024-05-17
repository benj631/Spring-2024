#Logistic Quiz 

library(mosaic)
library(dplyr)

View(Gestation)

?Gestation

Gestation <- Gestation %>%
  mutate(Has_Smoked = case_when(
    smoke == "never" ~ 0,
    smoke != "never" ~ 1
  ))


gest_glm <- glm(Has_Smoked ~ wt, data = Gestation, family = "binomial")
summary(gest_glm)
