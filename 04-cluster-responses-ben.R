# response clusters ------------------------------------------------------------
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(broom)

raw <- 
  read_rds("data-clean/raw.rds") %>% 
  na.omit()

set.seed(10)
# K-means ----------------------------------------------------------------------

# Step 1: Find the optimal number of clusters
# I wrote some code to get started

custom_kmeans <- function(clusters, data){
  fit <- kmeans(data, clusters, algorithm = "Lloyd")
  percent <- fit$betweenss / fit$totss
  tibble(clusters = clusters, fit = list(fit), percent = percent)
}

results <- 
  1:10 %>% 
  map_df(custom_kmeans, raw)

results %>% 
  ggplot(aes(x = clusters, y = percent)) +
  geom_point()
#>>> The plot reveals that the first 2 clusters explain ~88% of the withiness <<<#

# Step 2: Now fit k-means with the optimal number of clusters
kmout <- kmeans(results, 3)
kmout
# Step 3: Use the code that you sent me to interpret the clusters
kmout$centers %>% 
  as_tibble() %>% 
  select(1:10) %>% 
  mutate(id = row_number()) %>% 
  gather(var, val, -id) %>% 
  ggplot(aes(x = var, y = val, fill = as.factor(id))) +
  geom_col(position = "dodge") + 
  coord_flip()

#04 cluster responses for each set if questions
# For example, get conclusions such as "the first cluster is low on extroversion questions"

# Step 4: Look at how clusters vary across countries etc.

group_by(raw$country)
raw %>% 
  plot(color = country)