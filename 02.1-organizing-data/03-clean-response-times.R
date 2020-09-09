# times data 

# import libraries -------------------------------------------------------------
library(tidyverse)
library(naniar)
library(ggplot2)
library(sf)
library(gridExtra)

#set up ggplot world
library("rnaturalearth")
library("rnaturalearthdata")

# prepare time data ------------------------------------------------------------
#load data
r_time <- 
  read_rds("01.2-data-clean/dirty-data.rds") %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

replace_with_NA <- function(x){
  x[x < 500] <- NA
  x
} # replace_with_NA(c(1, 600, 1))

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

#aggregate raw_time to find average time taken to respond per country
raw_time <- time %>% 
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

# export -----------------------------------------------------------------------
r_time %>% 
  write_rds("01.2-data-clean/r_time.rds")

time %>% 
  filter(id %in% r_time$id) %>% 
  write_rds("01.2-data-clean/time.rds")

raw_time %>% 
  write_rds("01.2-data-clean/raw_time.rds")

# join joint and time data -----------------------------------------------------
joint <- joint %>% 
  left_join(raw_time, by = "country") 

# update world averages --------------------------------------------------------
# extract world data and join with joint
world <- 
  ne_countries(scale = "medium", returnclass = "sf") %>% 
  rename(country = sovereignt) %>% 
  left_join(joint, by = "country") %>%
  # drop_na(c(avg_EXT:science)) %>%
  # filter_at(
  #   vars(avg_EXT:science),
  #   ~ . != "..") %>% 
  as_tibble()

#plot time ---------------------------------------------------------------------
#plot average extroversion-time vs average extroversion per country
t1 <- ggplot(world) +
  geom_point(aes(x = avg_EXT_E, y = avg_EXT, color = continent)) +
  geom_smooth(aes(x = avg_EXT_E, y = avg_EXT), method = "lm") +
  geom_text(aes(x = avg_EXT_E, y = avg_EXT, label = country)) +
  ggsave("plots/extraversion-time~extroversion.png")

#plot average neuroticism-time vs average neuroticism per country
t2 <- ggplot(world) +
  geom_point(aes(x = avg_EST_E, y = avg_EST, color = continent)) +
  geom_smooth(aes(x = avg_EST_E, y = avg_EST), method = "lm") +
  geom_text(aes(x = avg_EST_E, y = avg_EST, label = country)) +
  ggsave("plots/neuroticism-time~neuroticism.png")

#plot average agreeableness-time vs average agreeableness per country
t3 <- ggplot(world) +
  geom_point(aes(x = avg_AGR_E, y = avg_AGR, color = continent)) +
  geom_smooth(aes(x = avg_AGR_E, y = avg_AGR), method = "lm") +
  geom_text(aes(x = avg_AGR_E, y = avg_AGR, label = country)) +
  ggsave("plots/agreeableness-time~agreeableness.png")

#plot average conscientiousness-time vs average conscientiousness per country
t4 <- ggplot(world) +
  geom_point(aes(x = avg_CSN_E, y = avg_CSN, color = continent)) +
  geom_smooth(aes(x = avg_CSN_E, y = avg_CSN), method = "lm") +
  geom_text(aes(x = avg_CSN_E, y = avg_CSN, label = country)) +
  ggsave("plots/conscientiousness-time~conscientiousness.png")

#plot average openness-time vs average openness per country
t5 <- ggplot(world) +
  geom_point(aes(x = avg_OPN_E, y = avg_OPN, color = continent)) +
  geom_smooth(aes(x = avg_OPN_E, y = avg_OPN), method = "lm") +
  geom_text(aes(x = avg_OPN_E, y = avg_OPN, label = country)) +
  ggsave("plots/openness-time~openness.png")

#setup plot layout
grid.arrange(t1, t2, t3, t4, t5, nrow = 3)
