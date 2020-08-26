# response clusters ------------------------------------------------------------
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(broom)

raw <- 
  read_rds("data-clean/raw.rds") %>% 
  na.omit() %>% 
  select(-id) %>% 
  sample_frac(0.01)

set.seed(5)
# PCA --------------------------------------------------------------------------
pr.out=prcomp(raw)
names(pr.out)
dim(raw)
dim(pr.out$x) #check that we now have a 2 x n matrix
pr.out$center
pr.out$rotation
pr.out$sdev

pr.out$x

#  variance explained by each principal component
(pr.var=pr.out$sdev ^2)
(pve=pr.var/sum(pr.var))


# manually perform PCA ---------------------------------------------------------
# as prcomp does not seem to have any effect
traw <- as_tibble(t(raw))
dim(traw)
dim(raw)
raw_cov <- as_tibble(cov(raw))
dim(raw_cov)
eg <- as_tibble(eigen(raw_cov)$vectors) 
names(raw_cov)
raw_cov
dim(eg)
eg <- eg %>% 
  select(1:2) %>% 
  t() %>% 
  as.matrix()
dim(eg)
raw <- raw %>% 
  as_tibble() %>% 
  as.matrix()
dim(raw)
mm <- raw%*%t(eg) #matrix multiplication result for PCA from raw, compressed for visualization
plot(mm) # plot resulting from PCA

# from this graph we can determine that 2 clusters does seem to reflect a good portion of the data
# We can use kmeans on "raw" using 2 clusters