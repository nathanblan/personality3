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
