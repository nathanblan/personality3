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
  mutate(id = row_number()) %>% 
  select(id, everything())


View(r_data)
names(r_data)


# - keyed ----------------------------------------------------------------------
r_data <- r_data %>% 
  select(id:OPN10) %>% 
  mutate_at( # mutate_at applies a function to each column you tell it to
    vars(EXT2, EXT4, EXT6,
         EXT8, EXT10, EST1,
         EST3, EST5, EST6,
         EST7, EST8, EST9,
         EST10, AGR1, AGR3,
         AGR5, AGR7, CSN2,
         CSN4, CSN6, CSN8,
         OPN2, OPN4, OPN6), # within vars() you'll want to list all reverse coded questions
    ~ recode(., `1` = 5, `2` = 4, `4` = 2, `5` = 1)
  )
View(r_data)

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

# classification models --------------------------------------------------------

