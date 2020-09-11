# response clusters ------------------------------------------------------------
library(ggbiplot)
library(tidyverse)  # data manipulation

#import data -------------------------------------------------------------------
raw <- 
  read_rds("01.2-data-clean/raw.rds") %>% 
  na.omit() %>% 
  sample_frac(0.01)


# PCA --------------------------------------------------------------------------
#compute PCA on raw
raw.pca <- prcomp(raw %>% 
                    select(-id, -country))

#calculate cumulative std. deviation to identify useful components
cum_perc_var_explained <- cumsum(raw.pca$sdev / sum(raw.pca$sdev))

tibble(
  component = 1:length(cum_perc_var_explained),
  cum_perc_var_explained) %>%
  #Look for elbow, find number of components / clusters explaining large portions of the data
  ggplot(aes(x = component, y  = cum_perc_var_explained)) +
  geom_point()

#extract the first two principle components
pca_sums <- as_tibble(raw.pca$x[,1:2]) %>% 
  mutate(country = raw$country)

#plot PCA
ggbiplot(raw.pca, alpha = 0.01, varname.size = 10) +
  ggsave("plots/pca-loadings.png")
raw.pca$rotation[,1:2] #each arrow is a point formed by the values in this chart
                       #PC1 = x PC2 = y

#summarize PCA
pca_sums %>% 
  group_by(country) %>% 
  dplyr::summarise(mean_pc1 = mean(PC1),
            mean_pc2 = mean(PC2))

#plot PC1 by country 
p1 <- ggplot(pca_sums) + 
  geom_point(aes(x = country, y= PC1)) +
  coord_flip()

#plot PC2 by country 
p2 <- ggplot(pca_sums) + 
  geom_point(aes(x = country, y= PC2)) +
  coord_flip()

grid.arrange(p1, p2, nrow = 1)

# update world averages --------------------------------------------------------
# extract world data and join with joint
world <- world %>% 
  left_join(time_sums, by = "country") %>%
  as_tibble()
