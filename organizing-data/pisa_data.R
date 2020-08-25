#PISA Data

#load libraries ----------------------------------------------------------------
library(tidyverse)
library(naniar)

#load PISA data ----------------------------------------------------------------
pisa <- 
  read_csv("data-raw/pisa-data.csv")
View(pisa)
