# Raw responses
# load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(modelr)

# load data
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

# response distributions
(EXT1_data <- r_data %>% 
   count(EXT1) %>% 
   filter(EXT1 != 0)) 
ggplot(data = EXT1_data) +
  geom_bar(mapping = aes(x=EXT1, y=n), stat = "identity")
