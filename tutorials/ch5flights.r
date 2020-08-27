library(nycflights13)
library(tidyverse)

View(flights)

flights %>% 
  filter(month == 1, day == 1)
(842/ 336776)

flights %>% 
  count(year, month, day) %>% 
  ggplot(aes(x=n)) + 
  geom_histogram()
  #count is number of days
  #x is flights
  #there are ~70 days with ~900 flights

jan1 <- filter(flights, month == 1, day == 1)
near(sqrt(2) ^ 2,  2)
#> [1] TRUE
near(1 / 49 * 49, 1)
#> [1] TRUE
filter(flights, month == 11 | month == 12)
nov_dec <- filter(flights, month %in% c(11, 12))
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

names(flights)

flights %>% 
  filter(arr_delay >= 120)

flights %>% 
  filter(dest == "IAH" | dest == "HOU")

(flights %>% 
  filter(carrier %in% c("United", "American", "Delta")))
?between

arrange(flights, year, month, day)
arrange(flights, desc(dep_delay))
arrange(flights, is.na(flights))
?one_of

flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)
transmute(flights, #only keeps new variables
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)
