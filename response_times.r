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
  na.omit() %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

#subset time response data
r_time <- r_time %>% 
  select(52:101) 
View(r_time)
names(r_time)