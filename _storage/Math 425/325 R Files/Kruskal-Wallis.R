library(pander)
library(plotly)
library(dplyr)
library(tidyverse)
library(mosaic)
library(car)

View(Friendly)


kruskal.test(correct ~ condition, data = Friendly)


qqSFR <- qqPlot(subset(Friendly[Friendly$condition == 'SFR']$correct))
qqBefore <- qqPlot(subset(Friendly[Friendly$condition == 'Before']$correct))
qqMeshed <- qqPlot(subset(Friendly[Friendly$condition == 'Meshed']$correct))

par(mfrow=c(1,1))



wilcox.test(filter(Friendly,
                   Friendly$condition == "Meshed")$correct,
            filter(Friendly, Friendly$condition == "Before")$correct,
            mu = 0, alternative = "two.sided",
            conf.level = 0.95)


wilcox.test(filter(Friendly,
                   Friendly$condition == "SFR")$correct,
            filter(Friendly, Friendly$condition == "Before")$correct,
            mu = 0, alternative = "two.sided",
            conf.level = 0.95)


wilcox.test(filter(Friendly,
                   Friendly$condition == "Meshed")$correct,
            filter(Friendly, Friendly$condition == "SFR")$correct,
            mu = 0, alternative = "two.sided",
            conf.level = 0.95)

# Mobius

?SaratogaHouses
View(SaratogaHouses)
table(SaratogaHouses$fuel)

kruskal.test(SaratogaHouses$price ~ SaratogaHouses$fuel)


boxplot(price ~ fuel, SaratogaHouses)

medgas <- median(subset(SaratogaHouses$price, SaratogaHouses$fuel == "gas"))
print(medgas)      
