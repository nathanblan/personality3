# time PCA----------------------------------------------------------------------
library(ggbiplot)
library(tidyverse)  # data manipulation

#import data -------------------------------------------------------------------
raw <- 
  read_rds("01.2-data-clean/time.rds") %>% 
  na.omit() %>% 
  sample_frac(0.01)


# PCA --------------------------------------------------------------------------
#compute PCA on raw
time.pca <- prcomp(raw %>% 
                    select(-id, -country) %>% 
                    log())

#calculate cumulative std. deviation to identify useful components
cum_perc_var_explained <- cumsum(time.pca$sdev / sum(time.pca$sdev))

tibble(
  component = 1:length(cum_perc_var_explained),
  cum_perc_var_explained) %>%
  #Look for elbow, find number of components / clusters explaining large portions of the data
  ggplot(aes(x = component, y  = cum_perc_var_explained)) +
  geom_point()

#extract the first two principle components
pca_sums_e <- as_tibble(time.pca$x[,1:2]) %>% 
  mutate(country = raw$country)

#plot PCA
ggbiplot(time.pca, alpha = 0.01, varname.size = 10) +
  ggsave("plots/pca-loadings.png")
time.pca$rotation[,1:2] #each arrow is a point formed by the values in this chart

#loadings ----------------------------------------------------------------------
#determine which questions are more prominent in each principle component
ve <- as_tibble(rownames(time.pca$rotation)) #row names stored on column: value

pc1_E_traits <- as_tibble(time.pca$rotation[,1:2]) %>% 
  mutate(trait = v$value) %>% 
  select(trait, everything()) %>% 
  arrange(desc(PC1))

pc2_E_traits <- as_tibble(time.pca$rotation[,1:2]) %>% 
  mutate(trait = v$value) %>% 
  select(trait, PC2, PC1) %>% 
  arrange(desc(PC2))

head(pc1_traits, 15)
head(pc2_traits, 15)

#summarize PCA -----------------------------------------------------------------
pca_sums_e <- pca_sums_e %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(mean_pc1_e = mean(PC1),
                   mean_pc2_e = mean(PC2)) 
head(pca_sums, 10)

#pca_sums plot by country
sums_plot <- pca_sums_e %>% 
  ggplot(aes(x = mean_pc1_e, y = mean_pc2, label = country)) +
  geom_point() +
  geom_text(label = pca_sums_e$country)
#geom_text_repel(min.segment.length = 0, seed = 1, box.padding = 0.5)
sums_plot

# update world averages --------------------------------------------------------
# extract world data and join with joint
world <- world %>% 
  left_join(pca_sums_e, by = "country") %>%
  as_tibble()

#plots -------------------------------------------------------------------------
#plot time pc1 vs average math, reading, and science per country
pc1.math_e <- ggplot(world) +
  geom_point(aes(x = mean_pc1_e, y = math, color = continent)) +
  coord_cartesian(xlim = c(-2.0, 3.0)) +
  geom_smooth(aes(x = mean_pc1_e, y = math), method = "lm") +
  geom_text(aes(x = mean_pc1_e, y = math, label = country)) +
  stat_regline_equation(
    aes(x = mean_pc1_e, y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 1, label.y = 350
  ) +
  stat_cor(
    aes(x = mean_pc1_e, y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = -1.5, label.y = 350
  ) +
  ggsave("plots/PC1~math.png")

pc1.science_e <- ggplot(world) +
  geom_point(aes(x = mean_pc1_e, y = science, color = continent)) +
  coord_cartesian(xlim = c(-2.0, 3.0)) +
  geom_smooth(aes(x = mean_pc1_e, y = science), method = "lm") +
  geom_text(aes(x = mean_pc1_e, y = science, label = country)) +
  stat_regline_equation(
    aes(x = mean_pc1_e, y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 1, label.y = 350
  ) +
  stat_cor(
    aes(x = mean_pc1_e, y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = -1.5, label.y = 350
  ) +
  ggsave("plots/PC1~science.png")

pc1.reading_e <- ggplot(world) +
  geom_point(aes(x = mean_pc1_e, y = reading, color = continent)) +
  coord_cartesian(xlim = c(-2.0, 3.0)) +
  geom_smooth(aes(x = mean_pc1_e, y = reading), method = "lm") +
  geom_text(aes(x = mean_pc1_e, y = reading, label = country)) +
  stat_regline_equation(
    aes(x = mean_pc1_e, y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 1, label.y = 350
  ) +
  stat_cor(
    aes(x = mean_pc1_e, y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = -1.5, label.y = 350
  ) +
  ggsave("plots/PC1~reading.png")

#plot time PC2 vs math, science, and reading
pc2.math_e <- ggplot(world) +
  geom_point(aes(x = mean_pc1_e, y = math, color = continent)) +
  coord_cartesian(xlim = c(-2.8, 2.5)) +
  geom_smooth(aes(x = mean_pc1_e, y = math), method = "lm") +
  geom_text(aes(x = mean_pc1_e, y = math, label = country)) +
  stat_regline_equation(
    aes(x = mean_pc1_e, y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 1, label.y = 350
  ) +
  stat_cor(
    aes(x = mean_pc1_e, y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = -1.5, label.y = 350
  ) +
  ggsave("plots/PC2~math.png")

pc2.science_e <- ggplot(world) +
  geom_point(aes(x = mean_pc1_e, y = science, color = continent)) +
  coord_cartesian(xlim = c(-2.8, 2.5)) +
  geom_smooth(aes(x = mean_pc1_e, y = science), method = "lm") +
  geom_text(aes(x = mean_pc1_e, y = science, label = country)) +
  stat_regline_equation(
    aes(x = mean_pc1_e, y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 1, label.y = 350
  ) +
  stat_cor(
    aes(x = mean_pc1_e, y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = -1.5, label.y = 350
  ) +
  ggsave("plots/PC2~science.png")

pc2.reading_e <- ggplot(world) +
  geom_point(aes(x = mean_pc1_e, y = reading, color = continent)) +
  coord_cartesian(xlim = c(-2.8, 2.5)) +
  geom_smooth(aes(x = mean_pc1_e, y = reading), method = "lm") +
  geom_text(aes(x = mean_pc1_e, y = reading, label = country)) +
  stat_regline_equation(
    aes(x = mean_pc1_e, y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 1, label.y = 350
  ) +
  stat_cor(
    aes(x = mean_pc1_e, y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = -1.5, label.y = 350
  ) +
  ggsave("plots/PC2~reading.png")

#setup plot layout pc1
grid.arrange(pc1.math, pc1.science, pc1.reading, nrow = 3)

#pc2
grid.arrange(pc2.math, pc2.science, pc2.reading, nrow = 3)
