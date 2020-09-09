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

# join time and PISA data ------------------------------------------------------
joint <- raw_time %>% 
  left_join(pisa_math, by = "country") %>% 
  left_join(pisa_read, by = "country") %>% 
  left_join(pisa_sci, by = "country") %>% 
  rename(science = `2015`,
         reading = `2015.y`,
         math = `2015.x`) %>% 
  select(-contains(c("c_code", "2013", "2014", "series", "s_code")))

# plot world averages ----------------------------------------------------------
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

#plots -------------------------------------------------------------------------
#plot world by average math
ggplot(data = world) +
  geom_sf(aes(fill = as.factor(math), geometry = geometry)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", 
          subtitle = paste0("(", length(unique(world$name)), " countries)")) +
  ggsave("plots/world-math-2015.png")

#plot world by average reading
ggplot(data = world) +
  geom_sf(aes(fill = as.factor(reading), geometry = geometry)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", 
          subtitle = paste0("(", length(unique(world$name)), " countries)")) +
  ggsave("plots/world-reading-2015.png")

#plot world by average science
ggplot(data = world) +
  geom_sf(aes(fill = as.factor(science), geometry = geometry)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", 
          subtitle = paste0("(", length(unique(world$name)), " countries)")) +
  ggsave("plots/world-sceince-2015.png")

#plot math ---------------------------------------------------------------------
#plot average extroversion-time vs average math per country
m1 <- ggplot(world) +
  geom_point(aes(x = avg_EXT_E, y = math, color = continent)) +
  geom_smooth(aes(x = avg_EXT_E, y = math), method = "lm") +
  geom_text(aes(x = avg_EXT_E, y = math, label = country)) +
  ggsave("plots/extraversion-time~math.png")

#plot average neuroticism-time vs average math per country
m2 <- ggplot(world) +
  geom_point(aes(x = avg_EST_E, y = math, color = continent)) +
  geom_smooth(aes(x = avg_EST_E, y = math), method = "lm") +
  geom_text(aes(x = avg_EST_E, y = math, label = country)) +
  ggsave("plots/neuroticism-time~math.png")

#plot average agreeableness-time vs average math per country
m3 <- ggplot(world) +
  geom_point(aes(x = avg_AGR_E, y = math, color = continent)) +
  geom_smooth(aes(x = avg_AGR_E, y = math), method = "lm") +
  geom_text(aes(x = avg_AGR_E, y = math, label = country)) +
  ggsave("plots/agreeableness-time~math.png")

#plot average conscientiousness-time vs average math per country
m4 <- ggplot(world) +
  geom_point(aes(x = avg_CSN_E, y = math, color = continent)) +
  geom_smooth(aes(x = avg_CSN_E, y = math), method = "lm") +
  geom_text(aes(x = avg_CSN_E, y = math, label = country)) +
  ggsave("plots/conscientiousness-time~math.png")

#plot average openness-time vs average math per country
m5 <- ggplot(world) +
  geom_point(aes(x = avg_OPN_E, y = math, color = continent)) +
  geom_smooth(aes(x = avg_OPN_E, y = math), method = "lm") +
  geom_text(aes(x = avg_OPN_E, y = math, label = country)) +
  ggsave("plots/openness~math-time.png")

#setup plot layout
grid.arrange(m1, m2, m3, m4, m5, nrow = 3)

#plot time ---------------------------------------------------------------------
#plot average extroversion-time vs average extroversion per country
r1 <- ggplot(world) +
  geom_point(aes(x = avg_EXT_E, y = avg_EXT, color = continent)) +
  geom_smooth(aes(x = avg_EXT_E, y = avg_EXT), method = "lm") +
  geom_text(aes(x = avg_EXT_E, y = avg_EXT, label = country)) +
  ggsave("plots/extraversion-time~extroversion.png")

#plot average neuroticism-time vs average neuroticism per country
r2 <- ggplot(world) +
  geom_point(aes(x = avg_EST_E, y = avg_EST, color = continent)) +
  geom_smooth(aes(x = avg_EST_E, y = avg_EST), method = "lm") +
  geom_text(aes(x = avg_EST_E, y = avg_EST, label = country)) +
  ggsave("plots/neuroticism-time~neuroticism.png")

#plot average agreeableness-time vs average agreeableness per country
r3 <- ggplot(world) +
  geom_point(aes(x = avg_AGR_E, y = avg_AGR, color = continent)) +
  geom_smooth(aes(x = avg_AGR_E, y = avg_AGR), method = "lm") +
  geom_text(aes(x = avg_AGR_E, y = avg_AGR, label = country)) +
  ggsave("plots/agreeableness-time~agreeableness.png")

#plot average conscientiousness-time vs average conscientiousness per country
r4 <- ggplot(world) +
  geom_point(aes(x = avg_CSN_E, y = avg_CSN, color = continent)) +
  geom_smooth(aes(x = avg_CSN_E, y = avg_CSN), method = "lm") +
  geom_text(aes(x = avg_CSN_E, y = avg_CSN, label = country)) +
  ggsave("plots/conscientiousness-time~conscientiousness.png")

#plot average openness-time vs average openness per country
r5 <- ggplot(world) +
  geom_point(aes(x = avg_OPN_E, y = avg_OPN, color = continent)) +
  geom_smooth(aes(x = avg_OPN_E, y = avg_OPN), method = "lm") +
  geom_text(aes(x = avg_OPN_E, y = avg_OPN, label = country)) +
  ggsave("plots/openness-time~openness.png")

#setup plot layout
grid.arrange(r1, r2, r3, r4, r5, nrow = 3)

#plot science ------------------------------------------------------------------
#plot average extroversion-time vs average science per country
s1 <- ggplot(world) +
  geom_point(aes(x = avg_EXT_E, y = science, color = continent)) +
  geom_smooth(aes(x = avg_EXT_E, y = science), method = "lm") +
  geom_text(aes(x = avg_EXT_E, y = science, label = country)) +
  ggsave("plots/extraversion-time~science.png")

#plot average neuroticism-time vs average science per country
s2 <- ggplot(world) +
  geom_point(aes(x = avg_EST_E, y = science, color = continent)) +
  geom_smooth(aes(x = avg_EST_E, y = science), method = "lm") +
  geom_text(aes(x = avg_EST_E, y = science, label = country)) +
  ggsave("plots/neuroticism-time~science.png")

#plot average agreeableness-time vs average science per country
s3 <- ggplot(world) +
  geom_point(aes(x = avg_AGR_E, y = science, color = continent)) +
  geom_smooth(aes(x = avg_AGR_E, y = science), method = "lm") +
  geom_text(aes(x = avg_AGR_E, y = science, label = country)) +
  ggsave("plots/agreeableness-time~science.png")

#plot average conscientiousness-time vs average science per country
s4 <- ggplot(world) +
  geom_point(aes(x = avg_CSN_E, y = science, color = continent)) +
  geom_smooth(aes(x = avg_CSN_E, y = science), method = "lm") +
  geom_text(aes(x = avg_CSN_E, y = science, label = country)) +
  ggsave("plots/conscientiousness-time~science.png")

#plot average openness-time vs average science per country
s5 <- ggplot(world) +
  geom_point(aes(x = avg_OPN_E, y = science, color = continent)) +
  geom_smooth(aes(x = avg_OPN_E, y = science), method = "lm") +
  geom_text(aes(x = avg_OPN_E, y = science, label = country)) +
  ggsave("plots/openness-time~science.png")

#setup plot layout
grid.arrange(s1, s2, s3, s4, s5, nrow = 3)
