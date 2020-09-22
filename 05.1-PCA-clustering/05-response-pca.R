# response PCA -----------------------------------------------------------------
library(ggbiplot)
library(tidyverse)  # data manipulation
set.seed(1)

#import data -------------------------------------------------------------------
raw <- 
  read_rds("01.2-data-clean/raw.rds") %>% 
  na.omit() %>% 
  sample_frac(0.01)


# PCA --------------------------------------------------------------------------
#compute PCA on raw
raw.pca <- prcomp(raw %>% 
                    select(-id, -country))
names(raw.pca)
#show loadings
raw.pca$x[,1:2]

#calculate cumulative std. deviation to identify useful components
cum_perc_var_explained <- cumsum(raw.pca$sdev / sum(raw.pca$sdev))

#plot cum. sdev
tibble(
  component = 1:length(cum_perc_var_explained),
  cum_perc_var_explained) %>%
  #Look for elbow, find number of components / clusters explaining large portions of the data
  ggplot(aes(x = component, y  = cum_perc_var_explained)) +
  geom_point()

#extract the first two principle components
pca_sums <- as_tibble(raw.pca$x[,1:2]) %>% 
  mutate(country = raw$country)
head(pca_sums, 10)

#plot PCA
ggbiplot(raw.pca, alpha = 0.01, varname.size = 10) +
  ggsave("plots/pca-loadings.png")
head(raw.pca$rotation[,1:2]) #each arrow is a point formed by the values in this chart
                             #PC1 = x PC2 = y

#loadings ----------------------------------------------------------------------
#determine which questions are more prominent in each principle component
v <- as_tibble(rownames(raw.pca$rotation)) #row names stored on column: value

pc1_traits <- as_tibble(raw.pca$rotation[,1:2]) %>% 
  mutate(trait = v$value) %>% 
  select(trait, everything()) %>% 
  arrange(desc(PC1))

pc2_traits <- as_tibble(raw.pca$rotation[,1:2]) %>% 
  mutate(trait = v$value) %>% 
  select(trait, PC2, PC1) %>% 
  arrange(desc(PC2))

head(pc1_traits, 15) 
head(pc2_traits, 15)

#summarize PCA -----------------------------------------------------------------
pca_sums <- pca_sums %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(mean_pc1 = mean(PC1),
            mean_pc2 = mean(PC2))
head(pca_sums, 10)

#pca_sums plot by country
sums_plot <- pca_sums %>% 
  ggplot(aes(x = mean_pc1, y = mean_pc2, label = country)) +
  geom_point() +
  geom_text(label = pca_sums$country)
sums_plot

# update world averages --------------------------------------------------------
# extract world data and join with joint
world <- world %>% 
  left_join(pca_sums, by = "country") %>%
  as_tibble()
names(world)

#plots -------------------------------------------------------------------------
#plot average extroversion vs average science per country
pc1.math <- ggplot(world) +
  geom_point(aes(x = mean_pc1, y = math, color = continent)) +
  geom_smooth(aes(x = mean_pc1, y = math), method = "lm") +
  geom_text(aes(x = mean_pc1, y = math, label = country)) +
  stat_regline_equation(
    aes(x = mean_pc1, y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 350
  ) +
  stat_cor(
    aes(x = mean_pc1, y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 350
  ) +
  ggsave("plots/PC1~math.png")

pc1.science <- ggplot(world) +
  geom_point(aes(x = mean_pc1, y = science, color = continent)) +
  geom_smooth(aes(x = mean_pc1, y = science), method = "lm") +
  geom_text(aes(x = mean_pc1, y = science, label = country)) +
  stat_regline_equation(
    aes(x = mean_pc1, y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 350
  ) +
  stat_cor(
    aes(x = mean_pc1, y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 350
  ) +
  ggsave("plots/PC1~science.png")

pc1.reading <- ggplot(world) +
  geom_point(aes(x = mean_pc1, y = reading, color = continent)) +
  geom_smooth(aes(x = mean_pc1, y = reading), method = "lm") +
  geom_text(aes(x = mean_pc1, y = reading, label = country)) +
  stat_regline_equation(
    aes(x = mean_pc1, y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 350
  ) +
  stat_cor(
    aes(x = mean_pc1, y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 350
  ) +
  ggsave("plots/PC1~reading.png")

#plot PC2 vs average math per country
pc2.math <- ggplot(world) +
  geom_point(aes(x = mean_pc2, y = math, color = continent)) +
  geom_smooth(aes(x = mean_pc2, y = math), method = "lm") +
  geom_text(aes(x = mean_pc2, y = math, label = country)) +
  stat_regline_equation(
    aes(x = mean_pc2, y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 350
  ) +
  stat_cor(
    aes(x = mean_pc2, y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 350
  ) +
  ggsave("plots/PC2~math.png")

pc2.science <- ggplot(world) +
  geom_point(aes(x = mean_pc2, y = science, color = continent)) +
  geom_smooth(aes(x = mean_pc2, y = science), method = "lm") +
  geom_text(aes(x = mean_pc2, y = science, label = country)) +
  stat_regline_equation(
    aes(x = mean_pc2, y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 350
  ) +
  stat_cor(
    aes(x = mean_pc2, y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 350
  ) +
  ggsave("plots/PC2~science.png")

pc2.reading <- ggplot(world) +
  geom_point(aes(x = mean_pc2, y = reading, color = continent)) +
  geom_smooth(aes(x = mean_pc2, y = reading), method = "lm") +
  geom_text(aes(x = mean_pc2, y = reading, label = country)) +
  stat_regline_equation(
    aes(x = mean_pc2, y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 350
  ) +
  stat_cor(
    aes(x = mean_pc2, y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 350
  ) +
  ggsave("plots/PC2~reading.png")

#setup plot layout pc1
grid.arrange(pc1.math, pc1.science, pc1.reading, nrow = 1)

#pc2
grid.arrange(pc2.math, pc2.science, pc2.reading, nrow = 3)

