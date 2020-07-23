#Raw responses
# load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(modelr)

# load data
responses <- 
  read_tsv("data-raw/data-final.csv") %>% 
  na.omit() %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

#subset raw response data
responses <- responses %>% 
  select(1:51) 
View(responses)
names(responses)
