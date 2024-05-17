?airquality
View(airquality)
mean(airquality$Wind)
sd(airquality$Wind)

median(airquality$Wind)
min(airquality$Wind)
max(airquality$Wind)

plot(Temp ~ Wind, data=airquality,ylab = "Wind", xlab = "Temp")

library(mosaic)
library(pander)
?KidsFeet

malefeet <- sum(KidsFeet$sex == "B")
malefeet

barplot(KidsFeet$birthmonth)
mode(KidsFeet$birthmonth)

getMode <- function(x) {
  uniq_x <- unique(x)
  tab <- tabulate(match(x, uniq_x))
  uniq_x[tab == max(tab)]
}

mode_birthmonth <- getMode(KidsFeet$birthmonth)

# Print the result
print(mode_birthmonth)

Kfdh <- table(KidsFeet$sex, KidsFeet$domhand)
Kfdh

summary_by_gender <- tapply(KidsFeet$length, KidsFeet$sex, summary)
summary_by_gender

gfootlength <- sd(subset(KidsFeet, sex == 'G')$length)
gfootlength

sd_length <- sd(subset(KidsFeet, sex == 'G')$length)
sd_length


bfL = sum(KidsFeet$biggerfoot == "L")
bfR = sum(KidsFeet$biggerfoot == "R")
bfL
bfR


barplot(table(airquality$Month, airquality$Temp),
        beside = TRUE,
        legend.text = TRUE,
        xlab = "Temp",
        ylab = "Month",
        col = rainbow(nlevels(factor(airquality$Month)))
        )

average_temps <- aggregate(Temp ~ Month, data = airquality, FUN = mean)

# Bar plot with adjusted width
barplot(average_temps$Temp, names.arg = average_temps$Month, col = rainbow(length(unique(airquality$Month))), width = 0.5)

# Add text labels above each bar
text(x = barplot(average_temps$Temp, names.arg = average_temps$Month, col = rainbow(length(unique(airquality$Month))), width = 0.5), 
     y = average_temps$Temp, 
     labels = round(average_temps$Temp, 2), 
     pos = 3, cex = 0.8, col = "black")


monthly_means <- aggregate(Temp ~ Month, data = airquality, FUN = mean)

# Find the mean temperature for July (assuming July is represented by the number 7)
mean_july <- monthly_means$Temp[monthly_means$Month == 7]

# Find the mean temperature for August (assuming August is represented by the number 8)
mean_august <- monthly_means$Temp[monthly_means$Month == 8]

# Subtract the mean temperature of July from the mean temperature of August
result <- mean_august - mean_july

# Print the result
print(result)



monthly_means <- aggregate(Temp ~ Month + Day, data = airquality, FUN = mean)

# Calculate the overall mean temperature for each month
overall_means <- aggregate(Temp ~ Month, data = monthly_means, FUN = mean)

# Print the result
print(overall_means)





#BADDD
hist(airquality$Temp, xlab="Daily Temperature", main="LaGuardia Airport (May to September 1973)", col="slategray")


plot(Temp ~ Month, data=airquality, xlab="Month", ylab="Temperature", main="LaGuardia Airport (May to September 1973)", pch=16, col="slategray")


boxplot(Temp ~ Month, data=airquality, xlab="Month", ylab="Temperature", main="LaGuardia Airport (May to September 1973)", pch=16, col="slategray")



stripchart(Temp ~ Month, data=airquality, ylab="Month", xlab="Temperature", main="LaGuardia Airport (May to September 1973)", pch=16, col="slategray", method="stack")


#BADDD
plot(Temp ~ Day, data=airquality, xlab="Day of the Month", ylab="Temperature", main="LaGuardia Airport (May to September 1973)", pch=16, col="slategray")


?Orange
?View(Orange)

selected_days <- c(484, 1004, 1372)

# Subset the data for the selected days
subset_data <- subset(Orange, age %in% selected_days)

# Calculate the median circumference for each selected day
median_circumference_by_day <- aggregate(circumference ~ age, data = subset_data, FUN = median)

# Print the result
print(median_circumference_by_day)




plot(circumference ~ age, data=Orange, ylab="Trunk Circumference (mm)", xlab="Age of Trees (days)", main="Trunk Circumference of Orange Trees", col="ivory3", pch=15)
Orange.m <- median(circumference ~ age, data=Orange)
lines(names(Orange.m), Orange.m, col="ivory3")
legend("topleft", legend="Median Growth", lty=1, col='ivory3', bty='n')

boxplot(circumference ~ age, data=Orange, ylab="Trunk Circumference (mm)", xlab="Age of Trees (days)", main="Trunk Circumference of Orange Trees", col="ivory3")


stripchart(circumference ~ age, data=Orange, ylab="Trunk Circumference (mm)", xlab="Age of Trees (days)", main="Trunk Circumference of Orange Trees", col="ivory3", pch=15, method="stack", vertical=TRUE)

# BADDDD
boxplot(Orange, xlab="Age of Tree (days)", main="Trunk Circumference of Orange Trees", col="ivory3")


?Riders

sum

daily_means <- aggregate(riders ~ day, data = Riders, FUN = sum)
daily_means
# Calculate the overall mean temperature for each month
overall_means <- aggregate(riders ~ day, data = daily_means, FUN = sum)

# Print the result
print(overall_means)


?mtcars
mtcars

average_mpg_cyl4 <- aggregate(mpg ~ am, data = subset(mtcars, cyl == 4), mean)

# Print the result
print(average_mpg_cyl4)


mean_qsec_cyl8_am <- mean(mtcars$qsec[mtcars$cyl == 8 & mtcars$am == 0])
mean_qsec_cyl8_manual <- mean(mtcars$qsec[mtcars$cyl == 8 & mtcars$am == 1])

# Print the results
print(paste("Mean Qsec for Automatic Transmission (am = 0):", round(mean_qsec_cyl8_am, 1)))
print(paste("Mean Qsec for Manual Transmission (am = 1):", round(mean_qsec_cyl8_manual, 1)))

heaviest_wt_cyl6_am <- max(mtcars$wt[mtcars$cyl == 6 & mtcars$am == 0])
hv6manwght <- max(mtcars$wt[mtcars$cyl == 6 & mtcars$am == 1])
diff = heaviest_wt_cyl6_am - hv6manwght
diff

# Print the result
print(paste("Weight of the heaviest 6-cylinder car with automatic transmission:", round(heaviest_wt_cyl6_am, 1), "thousands of pounds"))
