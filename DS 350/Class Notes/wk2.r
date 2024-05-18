library(dplyr)

?iris

iris$ratio <- iris %>% arrange(desc(Sepal.Length)) %>%
  mutate(
    ratio = Sepal.Length / Sepal.Width
  ) %>% select(Species,ratio) %>%
    head(7)
)
