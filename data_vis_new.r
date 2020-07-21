# load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)

# load data
p_data <- 
  read_tsv("data-raw/data-final.csv") %>% 
  na.omit() %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())
View(p_data)
