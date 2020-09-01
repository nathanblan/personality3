#Big 5 Data

# import libraries -------------------------------------------------------------
library(tidyverse)
library(naniar)
library(ggplot2)
library(sf)

#set up ggplot world
library("rnaturalearth")
library("rnaturalearthdata")

#define %notin% function
`%notin%` <- Negate(`%in%`)

# prepare big5 data ------------------------------------------------------------
#load data
r_data <- 
  read_tsv("01.1-data-raw/data-final.csv") %>% 
#  read_rds("01.2-data-clean/dirty-data.rds") %>% 
  mutate(id = row_number()) %>% 
  select(id, everything()) %>% 
  write_rds("01.2-data-clean/dirty-data.rds")

#join country data w/ r_data 
r_data <- r_data %>% 
  rename(c_code2 = country) %>% 
  left_join(codes, by = "c_code2") %>% 
  select(-c_code3) %>% 
  rename(country = countries)

"United States" %in% r_data$country

#remove "0" and na observations
r_data <- r_data %>%
  filter_all(all_vars(. != 0)) %>% 
  drop_na()

#check r_data stats
str(r_data)
names(r_data)
count(r_data)

#look for countries with a reasonable sample size
r_data %>% 
  count(country) %>% 
  ggplot(aes(x=n)) + 
  geom_histogram() +
  ggsave("plots/country_response_count.pdf")

# subset raw response data -----------------------------------------------------
raw <- 
  r_data %>% 
  select(EXT1:OPN10, country) %>% 
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
         OPN02, OPN04, OPN06), # within vars(), list all reverse coded questions
    ~ recode(., `1` = 5, `2` = 4, `4` = 2, `5` = 1)
  )

#aggregate raw to find average and median scores per trait per country
raw_sums <- raw %>% 
  group_by(country) %>% 
  summarise(
    avg_EXT = mean(c(EXT01:EXT10)),
    med_EXT = median(c(EXT01:EXT10)),
    avg_EST = mean(c(EST01:EST10)),
    med_EST = median(c(EST01:EST10)),
    avg_AGR = mean(c(AGR01:AGR10)),
    med_AGR = median(c(AGR01:AGR10)),
    avg_CSN = mean(c(CSN01:CSN10)),
    med_CSN = median(c(CSN01:CSN10)),
    avg_OPN = mean(c(OPN01:OPN10)),
    med_OPN = median(c(OPN01:OPN10)),
  )
raw_sums

# export -----------------------------------------------------------------------
r_data %>% 
  write_rds("01.2-data-clean/r_data.rds")

raw %>% 
  filter(id %in% r_data$id) %>% 
  write_rds("01.2-data-clean/raw.rds")

raw_sums %>% 
  write_rds("01.2-data-clean/raw_sums.rds")

# join big5 and PISA data ------------------------------------------------------
joint <- raw_sums %>% 
  left_join(pisa_math, by = "country") %>% 
  left_join(pisa_read, by = "country") %>% 
  left_join(pisa_sci, by = "country") %>% 
  rename(science = `2015`,
         reading = `2015.y`,
         math = `2015.x`) %>% 
  select(-contains(c("c_code", "2013", "2014", "series", "s_code")))

View(joint)

# plot world averages ----------------------------------------------------------
# extract world data and join with joint
world <- 
  ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(sovereignt == admin) %>% 
  rename(country = sovereignt) %>% 
  left_join(joint, by = "country") %>% 
# drop_na(c(avg_EXT:science)) %>%
# filter_at(
#   vars(avg_EXT:science),
#   ~ . != "..") %>% 
  as_tibble()

#plot world by average math
ggplot(data = world) +
  geom_sf(aes(fill = as.factor(math), geometry = geometry)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", 
          subtitle = paste0("(", length(unique(world$name)), " countries)")) +
  ggsave("plots/world-education-2015.pdf")
