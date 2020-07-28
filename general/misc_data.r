#other columns(?)
# load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(modelr)

# load data
misc <- 
  read_tsv("data-raw/data-final.csv") %>% 
  na.omit()

#subset time response data
misc <- misc %>% 
  select(dateload:long_appx_lots_of_err) %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())
View(misc)
names(misc)
