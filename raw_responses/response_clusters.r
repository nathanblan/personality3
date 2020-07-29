set.seed(1)
r_data_small <- r_data %>% 
  sample_frac(0.2)

k_model <- r_data_small %>% 
  select(-id) %>% 
  kmeans(centers = 800, iter.max = 100)

k_model
