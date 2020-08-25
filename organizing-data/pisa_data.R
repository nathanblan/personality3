#PISA Data

#load libraries ----------------------------------------------------------------
library(tidyverse)
library(naniar)

#load PISA data ----------------------------------------------------------------
pisa <- 
  read_tsv("data-raw/data-final.csv") %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())