curve(dnorm(x,-5,2), -12, 20, col="skyblue", lwd=2, ylab="", n=1000, ylim=c(0,.81))
curve(dchisq(x,5), from=0, add=TRUE, col="firebrick", lwd=2, n=1000)
curve(dt(x,5), add=TRUE, col="skyblue4", lwd=2, n=1000)
curve(df(x,10,10), from=0, add=TRUE, col="salmon", lwd=2, n=1000)
legend("topright", legend=c("Normal(-5,2)","Chi-squared(8)","t(5)","f(2,5)"), lwd=2, lty=1, col=c("skyblue","firebrick","skyblue4","salmon"), bty='n')


curve(dnorm(x, -5, 2), from=-12, to=2, lwd=2, col="skyblue", ylab="Normal Distribution with Mean of -5 and Standard Deviation of 2")
abline(h=0, v=-5, lty=2)

?CO2
View(CO2)
library(tidyverse)
library(ggplot2)
CO2.chilled.250 <- filter(CO2, Treatment == 'chilled' & conc == 250)


CO2_c250_q <= mean(subset(CO2.chilled.250, Type == 'Quebec')$uptake)
CO2_c250_q <= mean(subset(CO2.chilled.250, Type == 'Mississippi')$uptake)

CO2_c250_q <- subset(CO2.chilled.250, Type == 'Quebec')$uptake
CO2_c250_m <- subset(CO2.chilled.250, Type == 'Mississippi')$uptake

ggplot(data, aes(x = Category, y = Value)) +
  geom_point(size = 3)

CO2_c250_q <- subset(CO2.chilled.250, Type == 'Quebec')$uptake
CO2_c250_m <- subset(CO2.chilled.250, Type == 'Mississippi')$uptake

# Combine the data into a single dataframe
data <- data.frame(
  Group = rep(c("Quebec", "Mississippi"), times = c(length(CO2_c250_q), length(CO2_c250_m))),
  Value = c(CO2_c250_q, CO2_c250_m)
)

# Create a dot plot with count on the y-axis
ggplot(data, aes(x = Value, fill = Group)) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.7, binwidth = 1) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Dot Plot of Uptake for Quebec and Mississippi", x = "Uptake", y = "Count") +
  scale_fill_manual(values = c("Quebec" = "blue", "Mississippi" = "red"))

# Combine the data into a list
data <- list(Quebec = CO2_c250_q, Mississippi = CO2_c250_m)

# Create a dot plot
dotchart(unlist(data), main = "Dot Plot of Uptake for Quebec and Mississippi",
         cex = 0.7, pch = 16, col = rep(c("blue", "red"), sapply(data, length)),
         labels = rep(names(data), sapply(data, length)))

# Add legend
legend("topright", legend = names(data), fill = c("blue", "red"))

dotchart(
  x = list(Quebec = CO2_c250_q, Mississippi = CO2_c250_m),
  groups = rep(c("Quebec", "Mississippi"), each = 1),
  main = "Dot Plot of Uptake for Quebec and Mississippi",
  cex = 0.7,
  pch = 16,
  col = c("blue", "red"),
  labels = rep(names(list(Quebec = CO2_c250_q, Mississippi = CO2_c250_m)), each = 1)

  CO2_c250_q <- subset(CO2.chilled.250, Type == 'Quebec')$uptake
  CO2_c250_m <- subset(CO2.chilled.250, Type == 'Mississippi')$uptake
  
  # Create a dot plot with each group on its own line and switched axes
  stripchart(
    list(Quebec = CO2_c250_q, Mississippi = CO2_c250_m),
    method = "stack",
    main = "Dot Plot of Uptake for Quebec and Mississippi",
    pch = 16,
    col = c("blue", "red"),
    vertical = FALSE
  )
  
  # Add legend
  legend("topright", legend = c("Quebec", "Mississippi"), pch = 16, col = c("blue", "red"))
  