
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
p_data <- read_tsv("data-raw/data-final.csv")
#complete.cases(p_data)
p_data <- na.omit(p_data)
p_data <- tibble(p_data)
names(p_data)
#data summary
(data_sum <-summary(p_data))

#filter out all rows with elapsed time greater than 1 second
#p_data %>%
#  filter(testelapse > 1000 | EXT1 != 0)

time_summary <- p_data %>%
  summarise(avg_time = mean(testelapse),
            time_sd = sd(testelapse),
            min_time = min(testelapse),
            max_time = max(testelapse))


p_data <- p_data %>%
  #total score per row
  mutate(total_score = (EXT1 + EXT2 + EXT3 + EXT4 + EXT5 + EXT6 + EXT7 + EXT8 + EXT9 + EXT10
                      + EST1 + EST2 + EST3 + EST4 + EST5 + EST6 + EST7 + EST8 + EST9 + EST10
                      + AGR1 + AGR2 + AGR3 + AGR4 + AGR5 + AGR6 + AGR7 + AGR8 + AGR9 + AGR10
                      + CSN1 + CSN2 + CSN3 + CSN4 + CSN5 + CSN6 + CSN7 + CSN8 + CSN9 + CSN10
                      + OPN1 + OPN2+  OPN3 + OPN4 + OPN5 + OPN6 + OPN7 + OPN8 + OPN9 + OPN10))


p_data <- p_data %>%
  #total EXT score per row
  mutate(EXT_score = (EXT1 + EXT2 + EXT3 + EXT4 + EXT5 + EXT6 + EXT7 + EXT8 + EXT9 + EXT10))

p_data <- p_data %>%
  #total EST score per row
  mutate(EST_score = (EST1 + EST2 + EST3 + EST4 + EST5 + EST6 + EST7 + EST8 + EST9 + EST10))

p_data <- p_data %>%
  #total AGR score per row
  mutate(AGR_score = (AGR1 + AGR2 + AGR3 + AGR4 + AGR5 + AGR6 + AGR7 + AGR8 + AGR9 + AGR10))

p_data <- p_data %>%
  #total CSN score per row
  mutate(CSN_score = (CSN1 + CSN2 + CSN3 + CSN4 + CSN5 + CSN6 + CSN7 + CSN8 + CSN9 + CSN10))

p_data <- p_data %>%
  #total CSN score per row
  mutate(OPN_score = (OPN1 + OPN2+  OPN3 + OPN4 + OPN5 + OPN6 + OPN7 + OPN8 + OPN9 + OPN10))


#Response data for EXT1
(EXT1_data <- p_data %>% 
    count(EXT1) %>% 
    filter(EXT1 != 0)) 
ggplot(data = EXT1_data) + 
  geom_bar(mapping = aes(x=EXT1, y=n), stat = "identity")
###

#Response data for EXT2
(EXT2_data <- p_data %>% 
    count(EXT2) %>% 
    filter(EXT2 != 0)) 
ggplot(data = EXT2_data) + 
  geom_bar(mapping = aes(x=EXT2, y=n), stat = "identity")
###

#Response data for EXT3
(EXT3_data <- p_data %>% 
    count(EXT3) %>% 
    filter(EXT3 != 0)) 
ggplot(data = EXT3_data) + 
  geom_bar(mapping = aes(x=EXT3, y=n), stat = "identity")
###

#Response data for EXT4
(EXT4_data <- p_data %>% 
    count(EXT4) %>% 
    filter(EXT4 != 0)) 
ggplot(data = EXT4_data) + 
  geom_bar(mapping = aes(x=EXT4, y=n), stat = "identity")
###

View(p_data)
