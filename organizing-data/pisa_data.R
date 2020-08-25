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
names(pisa)[2] <- "c_code"
names(pisa)[3] <- "series"
names(pisa)[4] <- "s_code"
names(pisa)[5] <- "2013"
names(pisa)[6] <- "2014"
names(pisa)[7] <- "2015"

#filter for countries with entries
pisa <- pisa %>% 
  filter(2015 != "..")

#separate tests
#math
pisa_math <- pisa %>% 
  filter(s_code == "LO.PISA.MAT")

#reading
pisa_read <- pisa %>% 
  filter(s_code == "LO.PISA.REA")

#science
pisa_sci <- pisa %>% 
  filter(s_code == "LO.PISA.SCI")

# export as .rds
pisa %>% write_rds("data-clean/pisa.rds")

#load Country Code data --------------------------------------------------------
#read country code data
codes <- 
  read_csv("data-raw/country_code.csv") %>% 
  select(-X1)

