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
#compress centers vectors
dim(kmout$centers) # 2 50
dim(eg) # 2 50
kmcomp <- kmout$centers%*%t(eg) #compress centroid vectors
plot(x, data = NULL, class = NULL, size = 2,
     legend.position = c("right", "bottom", "left", "top", "none"),
     title = "K-Means Results", xlab = "Principal Component 1",
     ylab = "Principal Component 2")

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