#factor analysis

# import libraries -------------------------------------------------------------
library(ggfortify)
library(cluster)
library(fpc)
library(tidyverse)
library(naniar)
library(sf)
library(gridExtra)

#load data ---------------------------------------------------------------------
raw <- 
  read_rds(""01.2-data-clean/raw.rds"")
#set up clustering data --------------------------------------------------------
set.seed(1)
r_data_small <- r_data %>% 
  sample_frac(0.2) 

r_data_small <- na.omit(r_data_small)
r_data_small <- scale(r_data_small)
names(r_data_small)