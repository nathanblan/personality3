# response clusters ------------------------------------------------------------
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(broom)
library(ggbiplot)

raw <- 
  read_rds("01.2-data-clean/raw.rds") %>% 
  na.omit() %>% 
  sample_frac(0.01)

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

# PCA --------------------------------------------------------------------------
raw.pca <- prcomp(raw %>% 
                    select(-id, -country))
names(raw.pca)
cum_perc_var_explained <- cumsum(raw.pca$sdev / sum(raw.pca$sdev))

tibble(
  component = 1:length(cum_perc_var_explained),
  cum_perc_var_explained) %>%
  #Look for elbow, find number of components / clusters explaining large portions of the data
  ggplot(aes(x = component, y  = cum_perc_var_explained)) +
  geom_point()

extract_pca <- as_tibble(raw.pca$x[,1:2]) %>% 
  mutate(country = raw$country)

ggbiplot(raw.pca, alpha = 0.01, varname.size = 10) 

raw.pca$rotation[,1:2] %>% View()

extract_pca %>% 
  group_by(country) %>% 
  dplyr::summarise(mean_pc1 = mean(PC1),
            mean_pc2 = mean(PC2))