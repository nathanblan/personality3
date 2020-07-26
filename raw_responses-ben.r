# load packages and data --------------------------------------------------
library(tidyverse)
theme_set(theme_classic(base_size = 8))

r_data <- 
  read_tsv("data-raw/data-final.csv") %>% 
  na.omit() %>% 
  select(EXT1:OPN10) %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

# example plotting one column ---------------------------------------------
r_data %>% 
  ggplot(aes(x = as.factor(EXT1))) +
  geom_bar()

# plot all columns --------------------------------------------------------

# easy way (that just creates a plot in R)
r_data %>% 
  sample_frac(0.01) %>% 
  select(-id) %>% 
  gather(question, response) %>% 
  count(question, response) %>% 
  mutate(n = n / 1000) %>%  # lets work in thousands
  ggplot(aes(x = response, y = n)) +
  geom_col() +
  facet_wrap(~ question, scales = "free")

# harder way (that saves a PDF of all plots to desktop)
GG_save_pdf = function(list, filename){
  pdf(filename)
  for (p in list) {
    print(p)
  }
  
  dev.off()
  invisible(NULL)
}

f <- function(one_column){
  tibble(tmp = one_column) %>% 
    ggplot(aes(x = tmp)) +
    geom_bar()
}

all_plots <- list()
for (i in 2:ncol(r_data)) {
  all_plots[[i - 1]] <- f(r_data[[i]])
}

GG_save_pdf(all_plots, "~/Desktop/plots.pdf")
