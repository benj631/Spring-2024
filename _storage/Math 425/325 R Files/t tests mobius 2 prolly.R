library(mosaic)
View(Births78)

births78c = filter(Births78$day_of_week[count = 53])

library(dplyr)

# Group by day_of_week and count occurrences
day_counts <- Births78 %>%
  group_by(day_of_week) %>%
  summarise(count = n())

# Filter days with count equals 53
day_of_week_53 <- day_counts %>%
  filter(count == 53)

# Print the result
print(day_of_week_53)

days_with_53_occurrences <- Births78 %>%
  group_by(day_of_week) %>%
  filter(n() == 53) %>%
  select(day_of_week) %>%
  distinct()

# Filter the original dataset based on the days with 53 occurrences
births78c <- Births78 %>%
  filter(day_of_week %in% days_with_53_occurrences$day_of_week)

# Print the result
print(births78c)


# Answer

Births78 %>%
  
group_by(wday) %>%
  
summarise(n()) %>%
  
pander()

# Answer
View(Births78)

BirthsWedThu <- filter(Births78, wday %in% c("Wed","Thu"))

t.test(births ~ wday, data=BirthsWedThu, mu=0, alternative="two.sided", conf.level=0.95)

# Information

What two things are required to compute a p-value?

  A test statistic and a sampling distribution of the test statistic.
