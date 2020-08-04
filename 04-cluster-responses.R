library(tidyverse)
raw <- 
  read_rds("data-clean/raw.rds") %>% 
  na.omit()

# run code below with ALL data not just small
small <- raw %>% sample_frac(0.05) %>% select(-id)

# PCA
small.pca <- prcomp(small)
cum_perc_var_explained <- cumsum(small.pca$sdev / sum(small.pca$sdev))

tibble(
  component = 1:length(cum_perc_var_explained),
  cum_perc_var_explained
) %>% 
  ggplot(aes(x = component, y  = cum_perc_var_explained)) +
  geom_point()

# K means TO DO
fit <- kmeans(small, 3)
fit$centers %>% View()
