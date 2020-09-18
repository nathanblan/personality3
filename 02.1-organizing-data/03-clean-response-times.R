# times data 

# import libraries -------------------------------------------------------------
library(tidyverse)
library(naniar)

# prepare time data ------------------------------------------------------------
#load data
r_time <- 
  read_rds("01.2-data-clean/dirty-data.rds") 

#define function to replace with 500
replace_with_500 <- function(x){
  x[x < 500] <- 500
  x
}

#define function to replace with 300000
replace_with_300000 <- function(x){
  x[x > 300000] <- 300000
  x
}

#join country data w/ r_time 
r_time <- r_time %>% 
  rename(c_code2 = country) %>% 
  left_join(codes, by = "c_code2") %>% 
  select(-c_code3) %>% 
  rename(country = countries)

#remove "0" and na observations
r_time <- r_time %>%
  filter_all(all_vars(. != 0)) %>% 
  drop_na()

# subset time data -------------------------------------------------------------
time <- 
  r_time %>% 
  select(EXT1_E:OPN10_E, country) %>% 
  rename(
    EXT01_E = EXT1_E,
    EXT02_E = EXT2_E,
    EXT03_E = EXT3_E,
    EXT04_E = EXT4_E,
    EXT05_E = EXT5_E,
    EXT06_E = EXT6_E,
    EXT07_E = EXT7_E,
    EXT08_E = EXT8_E,
    EXT09_E = EXT9_E,
    OPN01_E = OPN1_E,
    OPN02_E = OPN2_E,
    OPN03_E = OPN3_E,
    OPN04_E = OPN4_E,
    OPN05_E = OPN5_E,
    OPN06_E = OPN6_E,
    OPN07_E = OPN7_E,
    OPN08_E = OPN8_E,
    OPN09_E = OPN9_E,
    AGR01_E = AGR1_E,
    AGR02_E = AGR2_E,
    AGR03_E = AGR3_E,
    AGR04_E = AGR4_E,
    AGR05_E = AGR5_E,
    AGR06_E = AGR6_E,
    AGR07_E = AGR7_E,
    AGR08_E = AGR8_E,
    AGR09_E = AGR9_E,
    EST01_E = EST1_E,
    EST02_E = EST2_E,
    EST03_E = EST3_E,
    EST04_E = EST4_E,
    EST05_E = EST5_E,
    EST06_E = EST6_E,
    EST07_E = EST7_E,
    EST08_E = EST8_E,
    EST09_E = EST9_E,
    CSN01_E = CSN1_E,
    CSN02_E = CSN2_E,
    CSN03_E = CSN3_E,
    CSN04_E = CSN4_E,
    CSN05_E = CSN5_E,
    CSN06_E = CSN6_E,
    CSN07_E = CSN7_E,
    CSN08_E = CSN8_E,
    CSN09_E = CSN9_E
  ) %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())
  
#filter out super low and super high times
time <- time %>% 
  select(-id, -country) %>% 
  replace_with_500() %>% 
  replace_with_300000 %>% 
  mutate(country = r_time$country, 
         id = r_time$id)

#aggregate time_sums to find average time taken to respond per country
time_sums <- time %>% 
  group_by(country) %>% 
  summarise(
    avg_EXT_E = mean(c(EXT01_E:EXT10_E)),
    med_EXT_E = median(c(EXT01_E:EXT10_E)),
    avg_EST_E = mean(c(EST01_E:EST10_E)),
    med_EST_E = median(c(EST01_E:EST10_E)),
    avg_AGR_E = mean(c(AGR01_E:AGR10_E)),
    med_AGR_E = median(c(AGR01_E:AGR10_E)),
    avg_CSN_E = mean(c(CSN01_E:CSN10_E)),
    med_CSN_E = median(c(CSN01_E:CSN10_E)),
    avg_OPN_E = mean(c(OPN01_E:OPN10_E)),
    med_OPN_E = median(c(OPN01_E:OPN10_E)),
  )
head(time_sums)# I don't think median is calculating correctly

# export -----------------------------------------------------------------------
r_time %>% 
  write_rds("01.2-data-clean/r_time.rds")

time %>% 
  filter(id %in% r_time$id) %>% 
  write_rds("01.2-data-clean/time.rds")

time_sums %>% 
  write_rds("01.2-data-clean/time_sums.rds")

# update world averages --------------------------------------------------------
# extract world data and join with joint
world <- world %>% 
  left_join(time_sums, by = "country") %>%
  as_tibble()
