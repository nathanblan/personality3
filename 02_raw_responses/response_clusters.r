# classification models --------------------------------------------------------
set.seed(1)
r_data_small <- r_data %>% 
  sample_frac(0.2) %>% 
  
  
  k_model <- r_data_small %>% 
  select(-id) %>% 
  kmeans(50, nstart = 50)

k_model$tot.withinss
# read islr and determine number of clusters we need, and how much of the data we really need to explain