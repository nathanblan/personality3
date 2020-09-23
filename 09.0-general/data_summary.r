#summaries
# load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(modelr)

# load data
data_sum <- 
  read_tsv("data-raw/data-final.csv") %>% 
  na.omit()

#create summaries for each of the other 3 groups
View(data_sum)
names(data_sum)

# summaries include basic stats on a column. example:
#time_summary <- p_data %>%
 # summarise(avg_time = mean(testelapse),
  #          time_sd = sd(testelapse),
   #         min_time = min(testelapse),
    #        max_time = max(testelapse))
# Each parameter (column) requires a sumamry and other summaries can be created as see fit
# Each data group (ie raw_respones) requires a summary
# Each subset in a group (ie raw_responses$EXT1) should get a summary

EXT1_sum <- data_sum %>%
  summarise(avg = mean(EXT1),
            response_sd = sd(EXT1))
