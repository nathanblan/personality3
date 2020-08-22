# response data ----------------------------------------------------------------

library(tidyverse)
library(naniar)

r_data <- 
  read_tsv("data-raw/data-final.csv") %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

raw <- 
  r_data %>% 
  select(EXT1:OPN10) %>% 
  rename(
    EXT01 = EXT1,
    EXT02 = EXT2,
    EXT03 = EXT3,
    EXT04 = EXT4,
    EXT05 = EXT5,
    EXT06 = EXT6,
    EXT07 = EXT7,
    EXT08 = EXT8,
    EXT09 = EXT9,
    OPN01 = OPN1,
    OPN02 = OPN2,
    OPN03 = OPN3,
    OPN04 = OPN4,
    OPN05 = OPN5,
    OPN06 = OPN6,
    OPN07 = OPN7,
    OPN08 = OPN8,
    OPN09 = OPN9,
    AGR01 = AGR1,
    AGR02 = AGR2,
    AGR03 = AGR3,
    AGR04 = AGR4,
    AGR05 = AGR5,
    AGR06 = AGR6,
    AGR07 = AGR7,
    AGR08 = AGR8,
    AGR09 = AGR9,
    EST01 = EST1,
    EST02 = EST2,
    EST03 = EST3,
    EST04 = EST4,
    EST05 = EST5,
    EST06 = EST6,
    EST07 = EST7,
    EST08 = EST8,
    EST09 = EST9,
    CSN01 = CSN1,
    CSN02 = CSN2,
    CSN03 = CSN3,
    CSN04 = CSN4,
    CSN05 = CSN5,
    CSN06 = CSN6,
    CSN07 = CSN7,
    CSN08 = CSN8,
    CSN09 = CSN9
  ) %>% 
  mutate(id = row_number()) %>% 
  select(id, everything()) %>% 
  mutate_at( # mutate_at applies a function to each column you tell it to
    vars(EXT02, EXT04, EXT06,
         EXT08, EXT10, EST01,
         EST03, EST05, EST06,
         EST07, EST08, EST09,
         EST10, AGR01, AGR03,
         AGR05, AGR07, CSN02,
         CSN04, CSN06, CSN08,
         OPN02, OPN04, OPN06), # within vars() you'll want to list all reverse coded questions
    ~ recode(., `1` = 5, `2` = 4, `4` = 2, `5` = 1)
  )

raw_sums <- 
  tibble(id = raw$id) %>% 
  mutate(
    EXT_sum = rowSums(raw[ , 2:11]),
    EST_sum = rowSums(raw[ , 12:21]),
    AGR_sum = rowSums(raw[ , 22:31]),
    CSN_sum = rowSums(raw[ , 32:41]),
    OPN_sum = rowSums(raw[ , 42:51])
  )

#filter for 0 observaions
r_data_0 <- r_data %>% 
  filter(r_data == 0)
View(r_data_0)
#! No observations containing 0 exist

# export
r_data %>% write_rds("data-clean/r_data.rds")
raw %>% write_rds("data-clean/raw.rds")
raw_sums %>% write_rds("data-clean/raw_sums.rds")

r_data %>% 
  names()
  
View(r_data$country)
