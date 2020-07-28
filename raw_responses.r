 # Raw responses
# load packages ----------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------
r_data <- 
  read_tsv("data-raw/data-final.csv") %>% 
  na.omit()

# subset raw response data
r_data <- r_data %>% 
  select(EXT1:OPN10) %>% 
  sample_frac(0.10) %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())
View(r_data)
names(r_data)

# add totals for each column ---------------------------------------------------
r_data$EXT_sum <- rowSums(r_data[,2:11])
r_data$EST_sum <- rowSums(r_data[,12:21])
r_data$AGR_sum <- rowSums(r_data[,22:31])
r_data$CSN_sum <- rowSums(r_data[,32:41])
r_data$OPN_sum <- rowSums(r_data[,42:51])

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
    ggtitle(sprintf("%s", r_data[[i]]))
}

response_plots <- list()
for (i in 2:ncol(r_data)) {
  response_plots[[i - 1]] <- f(r_data[[i]])
}

GG_save_pdf(response_plots, "response_plots.pdf")

# linear models ----------------------------------------------------------------
lm.fit=lm(EXT_sum∼EXT1:EXT10, data = r_data)
summary(lm.fit)
