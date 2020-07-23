#time responses r_time
# load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(modelr)

# load data
r_time <- 
  read_tsv("data-raw/data-final.csv") %>% 
  na.omit()

#subset time response data
r_time <- r_time %>% 
  select(EXT1_E:OPN10_E) %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())
View(r_time)
names(r_time)
