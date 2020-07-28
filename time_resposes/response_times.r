#time responses r_time
# load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(modelr)

# load data
r_time <- 
  read_tsv("data-raw/data-final.csv") %>% 
  na.omit()

#subset time response data
r_time <- r_time %>% 
  select(EXT1_E:OPN10_E) %>% 
  sample_frac(0.01) %>%
  mutate(id = row_number()) %>% 
  select(id, everything())
#View(r_time)
names(r_time)

# response distribution function -----------------------------------------------
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
    geom_bar() +
    ggtitle(sprintf("%s", r_data[[i]]))
}

time_plots <- list()
for (i in 2:ncol(r_data)) {
  time_plots[[i - 1]] <- f(r_data[[i]])
}

GG_save_pdf(time_plots, "time_plots.pdf")
