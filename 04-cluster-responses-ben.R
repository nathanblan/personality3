# response clusters ------------------------------------------------------------
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(broom)

raw <- 
  read_rds("data-clean/raw.rds") %>% 
  na.omit()

# K-means -------------------------------

# Step 1: Find the optimal number of clusters
# I wrote some code to get started

custom_kmeans <- function(clusters, data){
  fit <- kmeans(data, clusters, nstart = 10)
  percent <- fit$betweenss / fit$totss
  tibble(clusters = clusters, fit = list(fit), percent = percent)
}

results <- 
  1:10 %>% 
  map_df(custom_kmeans, raw)

results %>% 
  ggplot(aes(x = clusters, y = percent)) +
  geom_point()

# Step 2: Now fit k-means with the optimal number of clusters
kmout <- kmeans(results, 3)
# Step 3: Use the code that you sent me to interpret the clusters
#04 cluster responses for each set if questions
# For example, get conclusions such as "the first cluster is low on extroversion questions"

# Step 4: Look at how clusters vary across countries etc.

group_by(country)
plot(color = country)