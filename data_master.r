# load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(modelr)

# load data
p_data <- 
  read_tsv("data-raw/data-final.csv") %>% 
  na.omit() %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())
View(p_data)
names(p_data)

# split data into 4 data sets !! Add column "id" to all dataframes !!

# raw responses

# raw response time

# everything else

# summary stats

