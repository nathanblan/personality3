#PISA Data

#load libraries ----------------------------------------------------------------
library(tidyverse)
library(naniar)

#load PISA data ----------------------------------------------------------------
#read pisa data
pisa <- 
  read_csv("data-raw/pisa-data.csv")
View(pisa)

#rename columns

#filter for countries with entries
pisa <- pisa %>% 
  filter()

# export as .rds ---------------------------------------------------------------
r_data %>% write_rds("data-clean/r_data.rds")