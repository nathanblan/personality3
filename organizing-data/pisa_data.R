#PISA Data

#load libraries ----------------------------------------------------------------
library(tidyverse)
library(naniar)

#load PISA data ----------------------------------------------------------------
#read pisa data
pisa <- 
  read_csv("data-raw/pisa-data.csv")

#rename columns
names(pisa)[1] <- "country"
names(pisa)[2] <- "c-code"
names(pisa)[3] <- "series"
names(pisa)[4] <- "s-code"
names(pisa)[5] <- "2013"
names(pisa)[6] <- "2014"
names(pisa)[7] <- "2015"

#filter for countries with entries
pisa <- pisa %>% 
  filter(2015 != "..")

# export as .rds ---------------------------------------------------------------
r_data %>% write_rds("data-clean/r_data.rds")