---
title: "Chipotle Functions"
author: "Ben Jacobs"
execute:
  keep-md: TRUE
  df-print: paged
  warning: false
format:
  html:
    theme: cerulean
    code-fold: true
    code-line-numbers: true
    embed-resources: true
date: "2024-07-08"
---


::: {.cell}

```{.r .cell-code}
pacman::p_load(tidyverse,dplyr,lubridate,stringi,stringr)
```
:::

::: {.cell}

```{.r .cell-code}
string1 <- '{"Monday":94,"Tuesday":76,"Wednesday":89,"Thursday":106,"Friday":130,"Saturday":128,"Sunday":58}'

# Remove curly braces and split by comma
split_string <- str_remove_all(string1, "[{}]") %>%
  str_split(",") %>%
  unlist() # Atomic vector instead of list

# Split by colon, remove quotes, and create a data frame
data <- tibble(
  Day = map_chr(split_string, ~str_split(.x, ":")[[1]][1] %>% str_remove_all('"')),
  Visits = map_int(split_string, ~str_split(.x, ":")[[1]][2] %>% str_remove_all('"') %>% as.integer())
)

# View(data)

# a-Z includes the ascii from a-Z
# "[:alpha:]" includes all letters
```
:::

::: {.cell}

```{.r .cell-code}
# Function 1
Extract_Weekly_Visits <- function(string) {
  split_string <- str_remove_all(string, "[{}]") %>%
    str_split(",") %>%
    unlist() # Atomic vector instead of list

  # Split by colon, remove quotes, and create a data frame
  tib <- tibble(
    Day = map_chr(split_string, ~str_split(.x, ":")[[1]][1] %>% str_remove_all('"')),
    Visits = map_int(split_string, ~str_split(.x, ":")[[1]][2] %>% str_remove_all('"') %>% as.integer())
  )

  return(tib)
}
```
:::

::: {.cell}

```{.r .cell-code}
# Function 2
Most_Popular_Day <- function(string) {
  data <- Extract_Weekly_Visits(string)
  
  # Find the maximum number of visits
  max_visits <- max(data$Visits)
  
  # Find the day(s) with the maximum number of visits
  popular_days <- data %>% filter(Visits == max_visits) %>% pull(Day)
  
  # Return the most popular day(s) as a comma-separated string if there are ties
  return(paste(popular_days, collapse = ", "))
}
```
:::

::: {.cell}

```{.r .cell-code}
string1 <- '{"Monday":94,"Tuesday":76,"Wednesday":89,"Thursday":106,"Friday":130,"Saturday":128,"Sunday":58}'
string2 <- '{“Monday”:18,“Tuesday”:16,“Wednesday”:14,“Thursday”:27,“Friday”:26,“Saturday”:36,“Sunday”:20}'
string3 <- '{“Monday”:0,“Tuesday”:0,“Wednesday”:1,“Thursday”:0,“Friday”:0,“Saturday”:1,“Sunday”:0}'

print(Extract_Weekly_Visits(string1))
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 7 × 2
  Day       Visits
  <chr>      <int>
1 Monday        94
2 Tuesday       76
3 Wednesday     89
4 Thursday     106
5 Friday       130
6 Saturday     128
7 Sunday        58
```


:::

```{.r .cell-code}
print(Most_Popular_Day(string1))
```

::: {.cell-output .cell-output-stdout}

```
[1] "Friday"
```


:::

```{.r .cell-code}
print(Extract_Weekly_Visits(string2))
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 7 × 2
  Day         Visits
  <chr>        <int>
1 “Monday”        18
2 “Tuesday”       16
3 “Wednesday”     14
4 “Thursday”      27
5 “Friday”        26
6 “Saturday”      36
7 “Sunday”        20
```


:::

```{.r .cell-code}
print(Most_Popular_Day(string2))
```

::: {.cell-output .cell-output-stdout}

```
[1] "“Saturday”"
```


:::

```{.r .cell-code}
print(Extract_Weekly_Visits(string3))
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 7 × 2
  Day         Visits
  <chr>        <int>
1 “Monday”         0
2 “Tuesday”        0
3 “Wednesday”      1
4 “Thursday”       0
5 “Friday”         0
6 “Saturday”       1
7 “Sunday”         0
```


:::

```{.r .cell-code}
print(Most_Popular_Day(string3))
```

::: {.cell-output .cell-output-stdout}

```
[1] "“Wednesday”, “Saturday”"
```


:::
:::
