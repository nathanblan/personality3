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

# response distributions -------------------------------------------------------
(EXT1_data <- r_data %>% 
   count(EXT1) %>% 
   filter(EXT1 != 0)) 
ggplot(data = EXT1_data) +
  geom_bar(mapping = aes(x=EXT1, y=n), stat = "identity")

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
  column_data <- r_data %>% 
    count(one_column) %>% 
    filter(one_column != 0)
    ggplot(column_data) +
      geom_bar(mapping = aes(x=one_column, y=n), stat = "identity")
}

allplots <- map(r_data, f)
View(allplots)
GG_save_pdf(allplots, "plots.pdf")

# ------------------------------------------------------------------------------