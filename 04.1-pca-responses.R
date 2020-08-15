# response clusters ------------------------------------------------------------
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(broom)

raw <- 
  read_rds("data-clean/raw.rds") %>% 
  na.omit()

set.seed(10)
# PCA --------------------------------------------------------------------------
pr.out=prcomp(USArrests , scale=TRUE)