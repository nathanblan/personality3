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

# 1 raw responses

# 2 response time

# 3 everything else

# 4 summary stats

