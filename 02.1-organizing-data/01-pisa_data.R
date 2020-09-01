#PISA Data

#load libraries ----------------------------------------------------------------
library(tidyverse)
library(naniar)

#load PISA data ----------------------------------------------------------------
#read pisa data
pisa <- 
  read_csv("01.1-data-raw/pisa-data.csv")

#rename columns
names(pisa)[1] <- "country"
names(pisa)[2] <- "c_code3"
names(pisa)[3] <- "series"
names(pisa)[4] <- "s_code"
names(pisa)[5] <- "2013"
names(pisa)[6] <- "2014"
names(pisa)[7] <- "2015"
names(pisa)
#load Country Code data --------------------------------------------------------
#read country code data
codes <- 
  read_csv("01.1-data-raw/country_code.csv") %>% 
  select(-X1)
names(codes)

#rename columns
names(codes)[1] <- "countries"
names(codes)[2] <- "c_code2"
names(codes)[3] <- "c_code3"

#join country data w/ pisa
pisa <- pisa %>% 
  left_join(codes, by = "c_code3") %>% 
  select(-c_code2, -countries) 

#pisa data ---------------------------------------------------------------------
#filter for countries with entries
pisa <- pisa %>% 
  filter(2015 != "..")

#separate tests
#math
pisa_math <- pisa %>% 
  filter(s_code == "LO.PISA.MAT") %>% 
  arrange(country)

#reading
pisa_read <- pisa %>% 
  filter(s_code == "LO.PISA.REA") %>% 
  arrange(country)

#science
pisa_sci <- pisa %>% 
  filter(s_code == "LO.PISA.SCI") %>% 
  arrange(country)

# export as .rds
pisa %>% write_rds("01.2-data-clean/pisa.rds")
pisa_math %>% write_rds("01.2-data-clean/pisa_math.rds")
pisa_read %>% write_rds("01.2-data-clean/pisa_read.rds")
pisa_sci %>% write_rds("01.2-data-clean/pisa_sci.rds")

