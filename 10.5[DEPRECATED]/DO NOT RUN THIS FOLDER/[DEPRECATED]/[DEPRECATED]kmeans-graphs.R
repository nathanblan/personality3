# deprecated K means code

















# K means TO DO ----------------------------------------------------------------
# use cumsum of withiness to determine best number of clusters

# small
fit <- kmeans(raw, 2, nstart = 20)
assignments <- augment(fit, raw)

ggplot(data = assignments) +
  geom_point(aes(x = V1, y = V2, color = .cluster)) +
  labs(color = "Cluster Assignment",
       title = "K-Means Clustering Results with K = 2")

fit$centers %>% 
  as_tibble() %>% 
  mutate(id = row_number()) %>% 
  gather(var, val, -id) %>% 
  ggplot(aes(x = var, y = val, fill = as.factor(id))) +
  geom_col(position = "dodge") + 
  coord_flip()

# ext --------------------------------------------------------------------------
fit.ext <- kmeans(ext, 6)
fit.ext$centers %>% #plot means of each cluster, representing how high people each cluster scored
  as_tibble() %>% 
  select(EXT01:EXT10) %>% 
  mutate(id = row_number()) %>% 
  gather(var, val, -id) %>% 
  ggplot(aes(x = var, y = val, fill = as.factor(id))) +
  geom_col(position = "dodge") + 
  coord_flip()

# est --------------------------------------------------------------------------
fit.est <- kmeans(est, 5)
fit.est$centers %>% #plot means of each cluster, representing how high people each cluster scored
  as_tibble() %>% 
  select(EST01:EST10) %>% 
  mutate(id = row_number()) %>% 
  gather(var, val, -id) %>% 
  ggplot(aes(x = var, y = val, fill = as.factor(id))) +
  geom_col(position = "dodge") + 
  coord_flip()

# agr --------------------------------------------------------------------------
fit.agr <- kmeans(agr, 5)
fit.agr$centers %>% #plot means of each cluster, representing how high people each cluster scored
  as_tibble() %>% 
  select(AGR01:AGR10) %>% 
  mutate(id = row_number()) %>% 
  gather(var, val, -id) %>% 
  ggplot(aes(x = var, y = val, fill = as.factor(id))) +
  geom_col(position = "dodge") + 
  coord_flip()

# csn --------------------------------------------------------------------------
fit.csn <- kmeans(csn, 5)
fit.csn$centers %>% #plot means of each cluster, representing how high people each cluster scored
  as_tibble() %>% 
  select(CSN01:CSN10) %>% 
  mutate(id = row_number()) %>% 
  gather(var, val, -id) %>% 
  ggplot(aes(x = var, y = val, fill = as.factor(id))) +
  geom_col(position = "dodge") + 
  coord_flip()

# opn --------------------------------------------------------------------------
fit.opn <- kmeans(opn, 5)
fit.opn$centers %>% #plot means of each cluster, representing how high people each cluster scored
  as_tibble() %>% 
  select(OPN01:OPN10) %>% 
  mutate(id = row_number()) %>% 
  gather(var, val, -id) %>% 
  ggplot(aes(x = var, y = val, fill = as.factor(id))) +
  geom_col(position = "dodge") + 
  coord_flip()

# hierarchical clustering ------------------------------------------------------