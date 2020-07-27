 # Raw responses
# load packages ----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(modelr)
library(purrr)

# load data --------------------------------------------------------------------
r_data <- 
  read_tsv("data-raw/data-final.csv") %>% 
  na.omit()

# subset raw response data
r_data <- r_data %>% 
  select(EXT1:OPN10) %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())
View(r_data)
names(r_data)

# response distribution function -----------------------------------------------
r_data %>% 
  sample_frac(0.01) %>% 
  select(-id) %>% 
  gather(question, response) %>% 
  count(question, response) %>% 
  mutate(n = n / 1000) %>%  # lets work in thousands
  ggplot(aes(x = response, y = n)) +
  geom_col() +
  facet_wrap(~ question, scales = "free")

# linear models ----------------------------------------------------------------