View(CO2)
library(car)

CO2.chilled = subset(CO2, CO2$Treatment == "chilled")
CO2.chilled.250 <- subset(CO2.chilled,CO2.chilled$conc == 250)

CO2.chilled.250.q <- subset(CO2.chilled.250,CO2.chilled.250$type == "Quebec")
CO2.chilled.250.m <- subset(CO2.chilled.250,CO2.chilled.250$type == "Mississippi")

t.test(CO2.chilled.250.q$uptake, CO2.chilled.250.m$uptake, alternative = "two.sided")


CO2_chilled_250 <- subset(CO2, Treatment == "chilled" & conc == 250)

# Filter the data for Quebec and Mississippi groups
CO2_chilled_250_Quebec <- subset(CO2_chilled_250, Type == "Quebec")
CO2_chilled_250_Mississippi <- subset(CO2_chilled_250, Type == "Mississippi")

# Check if both groups have enough observations
nrow(CO2_chilled_250_Quebec)
nrow(CO2_chilled_250_Mississippi)

# Perform the t-test
t_test_result <- t.test(CO2_chilled_250_Quebec$uptake, CO2_chilled_250_Mississippi$uptake, alternative = "two.sided")

# Print the result
print(t_test_result)

View(CO2.chilled.250)
View(CO2_chilled_250_Quebec)
View(CO2_chilled_250_Mississippi)

qqPlot(uptake ~ Type, data = CO2.chilled.250)

data()
     