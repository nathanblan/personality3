# response clusters ------------------------------------------------------------
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(broom)

raw <- 
  read_rds("data-clean/raw.rds") %>% 
  na.omit() %>% 
  select(-id)

set.seed(5)
# PCA --------------------------------------------------------------------------
pr.out=prcomp(raw)
names(pr.out)
dim(raw)
dim(pr.out$x) #check that we now have a 2 x n matrix
pr.out$center
pr.out$rotation
pr.out$sdev

#  variance explained by each principal component
(pr.var=pr.out$sdev ^2)
(pve=pr.var/sum(pr.var))


#cov matrix?
traw <- as_tibble(t(raw))
dim(traw)
dim(raw)
raw_cov <- as_tibble(cov(raw))
range(eigen(raw_cov, only=T)$val) # 
names(raw_cov)
raw_cov
