#factor analysis
set.seed(1)

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
  read_rds("01.2-data-clean/raw.rds") %>% 
  select(-country)

#center variance of each column
raw_scaled <- apply(raw, 2, scale)
phi <- raw.eigen$vectors[,1:2]

#subset each trait
ext <- raw %>% 
  select(EXT01:EXT10)
est <- raw %>% 
  select(EST01:EST10)
agr <- raw %>% 
  select(AGR01:AGR10)
csn <- raw %>% 
  select(CSN01:CSN10)
opn <- raw %>% 
  select(OPN01:OPN10)

# calc. eigenvalues and vectors ------------------------------------------------
raw.cov <- cov(raw_scaled)
raw.eigen <- eigen(raw.cov)

#flip eigen sign
phi <- -phi

phi
#set up clustering data --------------------------------------------------------
set.seed(1)
r_data_small <- r_data %>% 
  sample_frac(0.2) 

r_data_small <- na.omit(r_data_small)
r_data_small <- scale(r_data_small)
names(r_data_small)