# DS350  Wk 4 Notes

library(tidyverse, warn.conflicts = FALSE)
wide_dat <- read_csv("https://byuistats.github.io/M335/data/rcw_reshape.csv")
print(spec(dat))

long_dat <- wide_dat %>%
  pivot_longer(cols = -c(Year, Semester, Semester_Date), 
               names_to = "Department", 
               values_to = "Count")

?pivot_longer

long_dat$Semester_Date %>% unique()
  
) %>%  mutate(
  Semester_Date = factor(Semester_Date, levels = c())
)

View(dat)


ggplot(long_dat, aes(x = Semester_Date, y = Count, color = Dept)) +
    geom_line()

# Separate, Unite, Paste

long_sep <- long_dat %>% 
    separate(col = Semester_Date, into = c("Day", "Month", "Year"), sep = '/', remove = FALSE) %>%
    unite("year_month", c(year, month), remove = FALSE) %>%
    mutate(
      new_column = paste(Year, Month, sep = "_")
      # new_column2 = substr(Semester, start = 1, end = 3)
    )



long_sep
