# response clusters ------------------------------------------------------------
library(tidyverse)
library(broom)

raw <- 
  read_rds("data-clean/raw.rds") %>% 
  na.omit()

ext <- raw %>% 
  select(EXT01:EXT10)
est <- raw %>% 
  select(EST01:EST10)
agr <- raw %>% 
  select(AGR01:AGR10)
csn <- raw %>% 
  select(CSN01:CSN10)
ext <- raw %>% 
  select(OPN01:OPN10)
# run code below with ALL data not just small
small <- raw %>% sample_frac(0.05) %>% select(-id)

# PCA
small.pca <- prcomp(small)
names(small.pca)
cum_perc_var_explained <- cumsum(small.pca$sdev / sum(small.pca$sdev))

tibble(
  component = 1:length(cum_perc_var_explained),
  cum_perc_var_explained
) %>% #Look for elbow, find number of components / clusters that explain large portions of the data
  ggplot(aes(x = component, y  = cum_perc_var_explained)) +
  geom_point()

# K means TO DO
names(small)

# use cumsum of withiness to determine best number of clusters

fit.ext <- kmeans(small, 5)
fit.ext$centers %>% #plot means of each cluster, representing how high people each cluster scored
  as_tibble() %>% 
  select(EXT01:EXT10) %>% 
  mutate(id = row_number()) %>% 
  gather(var, val, -id) %>% 
  ggplot(aes(x = var, y = val, fill = as.factor(id))) +
  geom_col(position = "dodge") + 
  coord_flip()

fit.est <- kmeans(small, 5)
fit.est$centers %>% #plot means of each cluster, representing how high people each cluster scored
  as_tibble() %>% 
  select(EST01:EST10) %>% 
  mutate(id = row_number()) %>% 
  gather(var, val, -id) %>% 
  ggplot(aes(x = var, y = val, fill = as.factor(id))) +
  geom_col(position = "dodge") + 
  coord_flip()

fit.agr <- kmeans(small, 5)
fit.agr$centers %>% #plot means of each cluster, representing how high people each cluster scored
  as_tibble() %>% 
  select(AGR01:AGR10) %>% 
  mutate(id = row_number()) %>% 
  gather(var, val, -id) %>% 
  ggplot(aes(x = var, y = val, fill = as.factor(id))) +
  geom_col(position = "dodge") + 
  coord_flip()

fit.csn <- kmeans(small, 5)
fit.csn
fit.csn$centers %>% #plot means of each cluster, representing how high people each cluster scored
  as_tibble() %>% 
  select(CSN01:CSN10) %>% 
  mutate(id = row_number()) %>% 
  gather(var, val, -id) %>% 
  ggplot(aes(x = var, y = val, fill = as.factor(id))) +
  geom_col(position = "dodge") + 
  coord_flip()

fit.opn <- kmeans(small, 5)
fit.opn$centers %>% #plot means of each cluster, representing how high people each cluster scored
  as_tibble() %>% 
  select(OPN01:OPN10) %>% 
  mutate(id = row_number()) %>% 
  gather(var, val, -id) %>% 
  ggplot(aes(x = var, y = val, fill = as.factor(id))) +
  geom_col(position = "dodge") + 
  coord_flip()
