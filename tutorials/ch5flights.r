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
