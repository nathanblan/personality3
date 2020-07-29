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


# bens suggested code -----------------------------------------------------
r_data %>% 
  select(id:OPN6) %>% 
  mutate_at( # mutate_at applies a function to each column you tell it to
    vars(EXT2, EXT4, OPN6), # within vars() you'll want to list all reverse coded questions
    ~ recode(., `1` = 5, `2` = 4, `4` = 2, `5` = 1)
  )


# - keyed ----------------------------------------------------------------------
inv_data <- r_data %>% 
  select(EXT2, EXT4, EXT6,
         EXT8, EXT10, EST1,
         EST3, EST5, EST6,
         EST7, EST8, EST9,
         EST10, AGR1, AGR3,
         AGR5, AGR7, CSN2,
         CSN4, CSN6, CSN8,
         OPN2, OPN4, OPN6)

inv_data <- inv_data %>% 
  mutate("EXT2" = recode(EXT2, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>% 
  mutate("EXT4" = recode(EXT4, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>% 
  mutate("EXT6" = recode(EXT6, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("EXT8" = recode(EXT8, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("EXT10" = recode(EXT10, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("EST1" = recode(EST1, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("EST3" = recode(EST3, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("EST5" = recode(EST5, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("EST6" = recode(EST6, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("EST7" = recode(EST7, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("EST8" = recode(EST8, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("EST9" = recode(EST9, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("EST10" = recode(EST10, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("AGR1" = recode(AGR1, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("AGR5" = recode(AGR5, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("AGR5" = recode(AGR5, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("AGR7" = recode(AGR7, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>% 
  mutate("CSN2" = recode(CSN2, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("CSN4" = recode(CSN4, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("CSN6" = recode(CSN6, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("CSN8" = recode(CSN8, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("OPN2" = recode(OPN2, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("OPN4" = recode(OPN4, `1` = 5, `2` = 4, `4` = 2, `5` = 1)) %>%
  mutate("OPN6" = recode(OPN6, `1` = 5, `2` = 4, `4` = 2, `5` = 1))

inv_data

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

