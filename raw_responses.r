 # Raw responses
# load packages ----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(modelr)
library(purrr)

# load data --------------------------------------------------------------------
r_data <- 
  read_tsv("data-raw/data-final.csv") %>% 
  na.omit()

# subset raw response data
r_data <- r_data %>% 
  select(EXT1:OPN10) %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())
View(r_data)
names(r_data)

# response distribution function -----------------------------------------------
# save as pdf
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
    ggtitle(one_column)
}

response_plots <- list()
for (i in 2:ncol(r_data)) {
  response_plots[[i - 1]] <- f(r_data[[i]])
}

GG_save_pdf(response_plots, "response_plots.pdf")

# linear models ----------------------------------------------------------------
