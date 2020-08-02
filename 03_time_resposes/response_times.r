#time responses r_time
# load packages
library(tidyverse)
library(naniar)
# load data
r_time <- 
  read_tsv("C:/personality3/01_data-raw/data-final.csv") %>% 
  na.omit()

#subset time response data
r_time <- r_time %>% 
  select(EXT1_E:OPN10_E) %>% 
  rename(
    EXT01_E = EXT1_E,
    EXT02_E = EXT2_E,
    EXT03_E = EXT3_E,
    EXT04_E = EXT4_E,
    EXT05_E = EXT5_E,
    EXT06_E = EXT6_E,
    EXT07_E = EXT7_E,
    EXT08_E = EXT8_E,
    EXT09_E = EXT9_E,
    OPN01_E = OPN1_E,
    OPN02_E = OPN2_E,
    OPN03_E = OPN3_E,
    OPN04_E = OPN4_E,
    OPN05_E = OPN5_E,
    OPN06_E = OPN6_E,
    OPN07_E = OPN7_E,
    OPN08_E = OPN8_E,
    OPN09_E = OPN9_E,
    AGR01_E = AGR1_E,
    AGR02_E = AGR2_E,
    AGR03_E = AGR3_E,
    AGR04_E = AGR4_E,
    AGR05_E = AGR5_E,
    AGR06_E = AGR6_E,
    AGR07_E = AGR7_E,
    AGR08_E = AGR8_E,
    AGR09_E = AGR9_E,
    EST01_E = EST1_E,
    EST02_E = EST2_E,
    EST03_E = EST3_E,
    EST04_E = EST4_E,
    EST05_E = EST5_E,
    EST06_E = EST6_E,
    EST07_E = EST7_E,
    EST08_E = EST8_E,
    EST09_E = EST9_E,
    CSN01_E = CSN1_E,
    CSN02_E = CSN2_E,
    CSN03_E = CSN3_E,
    CSN04_E = CSN4_E,
    CSN05_E = CSN5_E,
    CSN06_E = CSN6_E,
    CSN07_E = CSN7_E,
    CSN08_E = CSN8_E,
    CSN09_E = CSN9_E
  ) %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())
r_time[r_time == 0] <- NA
names(r_time)

# add totals for each column ---------------------------------------------------
r_data$EXT_sum <- rowSums(r_data[,2:11])
r_data$EST_sum <- rowSums(r_data[,12:21])
r_data$AGR_sum <- rowSums(r_data[,22:31])
r_data$CSN_sum <- rowSums(r_data[,32:41])
r_data$OPN_sum <- rowSums(r_data[,42:51])

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
    ggtitle(sprintf("%s", r_time[[i]]))
}

time_plots <- list()
for (i in 2:ncol(r_time)) {
  time_plots[[i - 1]] <- f(r_time[[i]])
}

GG_save_pdf(time_plots, "time_plots.pdf")
