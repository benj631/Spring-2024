library(pander)
library(plotly)
library(dyplr)
library(tidyverse)
library(mosaic)
library(car)

install.packages('dyplr')


pd.read_csv('https://github.com/byuidatascience/data4names/blob/master/data.md')

?mtcars

palette(c("skyblue","firebrick"))

plot(mpg ~ qsec, data=mtcars, col=as.factor(am), pch=16, xlab="Quarter Mile Time (seconds)", ylab="Miles per Gallon", main="1974 Motor Trend Vehicles")
legend("topright", pch=16, legend=c("automatic","manual"), title="Transmission", bty='n', col=palette())


newsum = (0.0004882812 + 0.0002441406 + 0.0002441406 + 0.0002441406)
View(newsum)


install.packages('car')



qqPlot(prestige ~ type, data = Duncan)

View(Duncan)

wilcox.test(prestige ~ type, data = Duncan, mu = 0, conf.level = 0.95)
wilcox.test(prestige ~ type, data = Duncan)

levels(Duncan$type)

subset_data <- subset(Duncan, type %in% c("prof", "wc"))

# Perform Wilcoxon rank sum test
wilcox.test(prestige ~ type, data = subset_data, mu = 0)

# THIS RIGHT HEREEEEEEEEEEE

subset_data <- subset(Duncan, type %in% c("prof", "wc"))

# Perform one-tailed Wilcoxon rank sum test
wilcox.test(prestige ~ type, data = subset_data, alternative = "greater")

library(ggplot2)

# Create the boxplot
ggplot(subset_data, aes(x = type, y = prestige, fill = type)) +
  geom_boxplot() +
  labs(title = "Comparison of Prestige between prof and wc",
       x = "Type",
       y = "Prestige") +
  theme_minimal()

summary(subset_data)


subset_wc <- subset(Duncan, type == "wc")
subset_prof <- subset(Duncan, type == "prof")

# Five-number summary for "wc" level
summary(subset_wc$prestige)
length(subset_wc$prestige)

# Five-number summary for "prof" level
summary(subset_prof$prestige)
length(subset_prof$prestige)


View(Salaries)

# Open the Salaries dataset in R. (Ensure the library(mosaic) is loaded.)
# As shown in the help file for this data set, ?Salaries, this data was collected from the 2008-09 academic school year for colleges in the U.S. "as part of the on-going effort of the college's administration to monitor salary differences between male and female faculty members."
# Also explained in the help file is that "rank" is "a factor with levels AssocProf AsstProf Prof". When trying to filter data, columns of data that are a "factor" can be frustrating to work with (as seen in the previous question). So sometimes it is easier to mutate(...) the data and change columns that are "factors" to "character".
# Change the "rank" column of the Salaries data from "factor" to "character" by running the commands:
  
  
  
Salaries2 <- Salaries %>%
  mutate(rank = as.character(rank))

# Subset the data for 'Prof' rank
prof_subset <- subset(Salaries2, rank == 'Prof')

# Calculate five-number summary by 'sex'
summary_by_sex <- tapply(prof_subset$salary, prof_subset$sex, summary)

# Calculate sample size by 'sex'
sample_size_by_sex <- table(prof_subset$sex)

# Print the results
print("Five-Number Summary by Sex:")
print(summary_by_sex)
print("Sample Size by Sex:")
print(sample_size_by_sex)

# Perform Wilcoxon rank sum test
prof_male <- subset(prof_subset, sex == 'Male')
prof_female <- subset(prof_subset, sex == 'Female')
wilcox.test(prof_male$salary, prof_female$salary, alternative = "two.sided", alpha = 0.05)

# Plotting
ggplot(prof_subset, aes(x = sex, y = salary, fill = sex)) +
  geom_boxplot() +
  labs(title = "Comparison of Salary between Male and Female Professors",
       x = "Sex",
       y = "Salary") +
  theme_minimal()


View(Davis)

# Subset Davis to only include men
DavisMen <- subset(Davis, sex == 'M')

# Compute the weight differences for men
DavisMen$rpdif <- DavisMen$weight - DavisMen$repwt

# Plot histogram of weight for the entire dataset
hist(Davis$weight, main = "Distribution of Weight", xlab = "Weight", ylab = "Frequency")

# Plot histogram of weight differences for men
hist(DavisMen$rpdif, main = "Distribution of Weight Differences for Men", xlab = "Weight Difference (Weight - Repwt)", ylab = "Frequency")


print(median(DavisMen$rpdif))

missing_values <- sum(is.na(DavisMen$rpdif))
print(paste("Number of missing values in rpdif column:", missing_values))

median_value <- median(DavisMen$rpdif, na.rm = TRUE)
print(median_value)

wilcox.test(DavisMen$rpdif, mu = 0, alternative = "two.sided")
wilcox.test(DavisMen$rpdif, mu = -0.5)

qqPlot(DavisMen$rpdif)



# Skills Test

View(Salaries)
asstSalaries = subset(Salaries,Salaries$rank == "AsstProf")
asstMale = subset(asstSalaries,sex == "Male")
asstFemale = subset(asstSalaries,sex == "Female")

print(length(asstMale$rank))
print(length(asstFemale$rank))

#2

boxplot(Salaries$salary ~ Salaries$discipline)


wilcox.test(Salaries$salary ~ Salaries$discipline, mu = 0)
