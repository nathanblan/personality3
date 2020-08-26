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

