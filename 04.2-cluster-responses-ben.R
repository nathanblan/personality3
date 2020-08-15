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

#>>> The plot reveals that the first 2 clusters explain ~75% of the withiness <<<#
names(kmout)

# Step 2: Now fit k-means with the optimal number of clusters
kmout <- kmeans(raw, 2)
plot(kmout$cluster)
assignments <- augment(kmout, raw)
names(assignments)
ggplot(data = assignments) +
  geom_point(aes(kmout, color = .cluster)) +
  labs(color = "Cluster Assignment",
       title = "K-Means Clustering Results with K = 2")

#compress centers vectors
kmcomp <- as_tibble(t(kmout$centers)%*%eg) #compress centroid vectors
compout <- kmeans(kmcomp, 2)
names(compout)
assignments <- augment(compout, kmcomp)
ggplot(data = assignments) +
  geom_point(aes(x = V1, y = V2, color = .cluster)) +
  labs(color = "Cluster Assignment",
       title = "K-Means Clustering Results with K = 2")

# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)
# Add Species groups from the original data sett
ind.coord$Species <- df$Species
# Data inspection
head(ind.coord)
# Step 3: Use the code that you sent me to interpret the clusters
final$centers %>% 
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

raw %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")