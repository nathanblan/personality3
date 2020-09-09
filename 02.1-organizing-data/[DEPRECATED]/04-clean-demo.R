# demographics data ------------------------------------------------------------
library(tidyverse)
r_data <- read_rds("data-clean/r_data.rds")

raw_demo <- 
  r_data %>% 
  select(id, dateload:long_appx_lots_of_err) %>% 
  select(id, country, everything())

raw_demo %>% write_rds("data-clean/raw_demo.rds")