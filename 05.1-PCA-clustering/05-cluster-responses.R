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


# PCA --------------------------------------------------------------------------
#compute PCA on raw
raw.pca <- prcomp(raw %>% 
                    select(-id, -country))

#calculate cumulative std. deviation to identify amount of 
cum_perc_var_explained <- cumsum(raw.pca$sdev / sum(raw.pca$sdev))

tibble(
  component = 1:length(cum_perc_var_explained),
  cum_perc_var_explained) %>%
  #Look for elbow, find number of components / clusters explaining large portions of the data
  ggplot(aes(x = component, y  = cum_perc_var_explained)) +
  geom_point()

#extract the first two principle components
extract_pca <- as_tibble(raw.pca$x[,1:2]) %>% 
  mutate(country = raw$country)

#plot PCA
ggbiplot(raw.pca, alpha = 0.01, varname.size = 10) 
raw.pca$rotation[,1:2] %>% View()

#summarise PCA
extract_pca %>% 
  group_by(country) %>% 
  dplyr::summarise(mean_pc1 = mean(PC1),
            mean_pc2 = mean(PC2))

#join PCA and joint/world
joint <- raw_sums %>% 
  left_join(pisa_math, by = "country") %>% 
  left_join(pisa_read, by = "country") %>% 
  left_join(pisa_sci, by = "country") %>% 
  rename(science = `2015`,
         reading = `2015.y`,
         math = `2015.x`) %>% 
  select(-contains(c("c_code", "2013", "2014", "series", "s_code")))
