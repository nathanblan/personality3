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
  dplyr::rename(c_code2 = country) %>% 
  left_join(codes, by = "c_code2") %>% 
  dplyr::select(-c_code3) %>% 
  dplyr::rename(country = countries)

#remove "0" and na observations
r_time <- r_time %>%
  filter_all(all_vars(. != 0)) %>% 
  drop_na()

# subset time data -------------------------------------------------------------
time <- 
  r_time %>% 
  dplyr::select(EXT1_E:OPN10_E, country) %>% 
  dplyr::rename(
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
  dplyr::mutate(id = row_number()) %>% 
  dplyr::select(id, everything())
  
#filter out super low and super high times
time <- time %>% 
  dplyr::select(-id, -country) %>% 
  replace_with_500() %>% 
  replace_with_300000 %>% 
  dplyr::mutate(country = r_time$country, 
         id = r_time$id)

#aggregate time_sums to find average/median time taken to respond per country
new_time <- time %>% 
  dplyr::select(-id) %>% 
  gather(var, val, -country) %>% 
  dplyr::group_by(country, question = str_sub(var, 1, 3)) %>% 
  dplyr::summarise(mean = mean(val), 
            median = median(val))

#summary of countries by average
avg_time <- new_time %>% 
  dplyr::select(-median) %>% 
  spread(question, mean) %>% 
  dplyr::rename(avg_EXT_E = EXT,
         avg_EST_E = EST,
         avg_AGR_E = AGR,
         avg_CSN_E = CSN,
         avg_OPN_E = OPN)

#summary of countries by median
med_time <- new_time %>% 
  dplyr::select(-mean) %>% 
  spread(question, median) %>% 
  dplyr::rename(med_EXT_E = EXT,
         med_EST_E = EST,
         med_AGR_E = AGR,
         med_CSN_E = CSN,
         med_OPN_E = OPN)

#join median and averages
time_sums <- avg_time %>% 
  left_join(med_time, by = "country")

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
names(world)
