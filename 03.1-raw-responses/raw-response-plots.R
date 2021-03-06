#response plots
#p values smaller than 0.01 -> probably relationship
# import libraries -------------------------------------------------------------
#set up ggplot world
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(ggpubr)

# plot world averages ----------------------------------------------------------
# extract world data and join with joint
world <- 
  ne_countries(scale = "medium", returnclass = "sf") %>% 
  dplyr::rename(country = sovereignt) %>% 
  left_join(joint, by = "country") %>%
  as_tibble()
names(world)

#plots -------------------------------------------------------------------------
#plot world by average math
w1 <- ggplot(data = world) +
  geom_sf(aes(fill = as.factor(math), geometry = geometry)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", 
          subtitle = paste0("(", length(unique(world$name)), " countries)")) +
  ggsave("plots/world-math-2015.png")

#plot world by average reading
w2 <- ggplot(data = world) +
  geom_sf(aes(fill = as.factor(reading), geometry = geometry)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", 
          subtitle = paste0("(", length(unique(world$name)), " countries)")) +
  ggsave("plots/world-reading-2015.png")

#plot world by average science
w3 <- ggplot(data = world) +
  geom_sf(aes(fill = as.factor(science), geometry = geometry)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", 
          subtitle = paste0("(", length(unique(world$name)), " countries)")) +
  ggsave("plots/world-sceince-2015.png")

grid.arrange(w1, w2, w3, nrow = 3)

#averages ======================================================================
#plot math ---------------------------------------------------------------------
#plot average extroversion vs average math per country
m1 <- ggplot(world) +
  geom_point(aes(x = avg_EXT, y = math, color = continent)) +
  coord_cartesian(xlim = c(2.555, 3.15)) +
  geom_smooth(aes(x = avg_EXT, y = math), method = "lm") +
  geom_text(aes(x = avg_EXT, y = math, label = country)) +
  stat_regline_equation(
    aes(x = avg_EXT, y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 310
  ) +
  stat_cor(
    aes(x = avg_EXT, y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 2.65, label.y = 310
  ) +
  ggsave("plots/extraversion~math.png")
m1

#plot average neuroticism vs average math per country
m2 <- ggplot(world) +
  geom_point(aes(x = avg_EST, y = math, color = continent)) +
  coord_cartesian(xlim = c(2.5, 3.2)) +
  geom_smooth(aes(x = avg_EST, y = math), method = "lm") +
  geom_text(aes(x = avg_EST, y = math, label = country)) +
  stat_regline_equation(
    aes(x = avg_EST, y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 320
  ) +
  stat_cor(
    aes(x = avg_EST, y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 2.75, label.y = 320
  ) +
  ggsave("plots/neuroticism~math.png")
m2

#plot average agreeableness vs average math per country
m3 <- ggplot(world) +
  geom_point(aes(x = avg_AGR, y = math, color = continent)) +
  coord_cartesian(xlim = c(3.3, 3.9)) +
  geom_smooth(aes(x = avg_AGR, y = math), method = "lm") +
  geom_text(aes(x = avg_AGR, y = math, label = country)) +
  stat_regline_equation(
    aes(x = avg_AGR, y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3.6, label.y = 320
  ) +
  stat_cor(
    aes(x = avg_AGR, y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 3.45, label.y = 320
  ) +
  ggsave("plots/agreeableness~math.png")
m3

#plot average conscientiousness vs average math per country
m4 <- ggplot(world) +
  geom_point(aes(x = avg_CSN, y = math, color = continent)) +
  coord_cartesian(xlim = c(3.1, 3.5)) +
  geom_smooth(aes(x = avg_CSN, y = math), method = "lm") +
  geom_text(aes(x = avg_CSN, y = math, label = country)) +
  stat_regline_equation(
    aes(x = avg_CSN, y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3.3, label.y = 320
  ) +
  stat_cor(
    aes(x = avg_CSN, y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 3.1, label.y = 320
  ) +
  ggsave("plots/conscientiousness~math.png")
m4

#plot average openness vs average math per country
m5 <- ggplot(world) +
  geom_point(aes(x = avg_OPN, y = math, color = continent)) +
  coord_cartesian(xlim = c(3.45, 4.1)) +
  geom_smooth(aes(x = avg_OPN, y = math), method = "lm") +
  geom_text(aes(x = avg_OPN, y = math, label = country)) +
  stat_regline_equation(
    aes(x = avg_OPN, y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3.8, label.y = 320
  ) +
  stat_cor(
    aes(x = avg_OPN, y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 3.5, label.y = 320
  ) +
  ggsave("plots/openness~math.png")
m5

#setup plot layout
grid.arrange(m1, m2, m3, m4, m5, nrow = 3)

#plot reading ------------------------------------------------------------------
#plot average extroversion vs average reading per country
r1 <- ggplot(world) +
  geom_point(aes(x = avg_EXT, y = reading, color = continent)) +
  coord_cartesian(xlim = c(2.55, 3.1)) +
  geom_smooth(aes(x = avg_EXT, y = reading), method = "lm") +
  geom_text(aes(x = avg_EXT, y = reading, label = country)) +
  stat_regline_equation(
    aes(x = avg_EXT, y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 2.8, label.y = 320
  ) +
  stat_cor(
    aes(x = avg_EXT, y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 2.65, label.y = 320
  ) +
  ggsave("plots/extraversion~reading.png")
r1

#plot average neuroticism vs average reading per country
r2 <- ggplot(world) +
  geom_point(aes(x = avg_EST, y = reading, color = continent)) +
  coord_cartesian(xlim = c(2.6, 3.1)) +
  geom_smooth(aes(x = avg_EST, y = reading), method = "lm") +
  geom_text(aes(x = avg_EST, y = reading, label = country)) +
  stat_regline_equation(
    aes(x = avg_EST, y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 2.9, label.y = 320
  ) +
  stat_cor(
    aes(x = avg_EST, y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 2.7, label.y = 320
  ) +
  ggsave("plots/neuroticism~reading.png")
r2

#plot average agreeableness vs average reading per country
r3 <- ggplot(world) +
  geom_point(aes(x = avg_AGR, y = reading, color = continent)) +
  coord_cartesian(xlim = c(3.4, 3.85)) +
  geom_smooth(aes(x = avg_AGR, y = reading), method = "lm") +
  geom_text(aes(x = avg_AGR, y = reading, label = country)) +
  stat_regline_equation(
    aes(x = avg_AGR, y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3.55, label.y = 320
  ) +
  stat_cor(
    aes(x = avg_AGR, y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 3.4, label.y = 320
  ) +
  ggsave("plots/agreeableness~reading.png")
r3

#plot average conscientiousness vs average reading per country
r4 <- ggplot(world) +
  geom_point(aes(x = avg_CSN, y = reading, color = continent)) +
  coord_cartesian(xlim = c(3.1, 3.5)) +
  geom_smooth(aes(x = avg_CSN, y = reading), method = "lm") +
  geom_text(aes(x = avg_CSN, y = reading, label = country)) +
  stat_regline_equation(
    aes(x = avg_CSN, y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3.3, label.y = 320
  ) +
  stat_cor(
    aes(x = avg_CSN, y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 3.15, label.y = 320
  ) +
  ggsave("plots/conscientiousness~reading.png")
r4

#plot average openness vs average reading per country
r5 <- ggplot(world) +
  geom_point(aes(x = avg_OPN, y = reading, color = continent)) +
  coord_cartesian(xlim = c(3.45, 4.15)) +
  geom_smooth(aes(x = avg_OPN, y = reading), method = "lm") +
  geom_text(aes(x = avg_OPN, y = reading, label = country)) +
  stat_regline_equation(
    aes(x = avg_OPN, y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3.8, label.y = 320
  ) +
  stat_cor(
    aes(x = avg_OPN, y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 3.5, label.y = 320
  ) +
  ggsave("plots/openness~reading.png")
r5

#setup plot layout
grid.arrange(r1, r2, r3, r4, r5, nrow = 3)

#plot science ------------------------------------------------------------------
#plot average extroversion vs average science per country
s1 <- ggplot(world) +
  geom_point(aes(x = avg_EXT, y = science, color = continent)) +
  coord_cartesian(xlim = c(2.6, 3.1)) +
  geom_smooth(aes(x = avg_EXT, y = science), method = "lm") +
  geom_text(aes(x = avg_EXT, y = science, label = country)) +
  stat_regline_equation(
    aes(x = avg_EXT, y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 2.8, label.y = 320
  ) +
  stat_cor(
    aes(x = avg_EXT, y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 2.65, label.y = 320
  ) +
  ggsave("plots/extraversion~science.png")
s1

#plot average neuroticism vs average science per country
s2 <- ggplot(world) +
  geom_point(aes(x = avg_EST, y = science, color = continent)) +
  coord_cartesian(xlim = c(2.6, 3.1)) +
  geom_smooth(aes(x = avg_EST, y = science), method = "lm") +
  geom_text(aes(x = avg_EST, y = science, label = country)) +
  stat_regline_equation(
    aes(x = avg_EST, y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 2.8, label.y = 320
  ) +
  stat_cor(
    aes(x = avg_EST, y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 2.65, label.y = 320
  ) +
  ggsave("plots/neuroticism~science.png")
s2

#plot average agreeableness vs average science per country
s3 <- ggplot(world) +
  geom_point(aes(x = avg_AGR, y = science, color = continent)) +
  coord_cartesian(xlim = c(3.35, 3.85)) +
  geom_smooth(aes(x = avg_AGR, y = science), method = "lm") +
  geom_text(aes(x = avg_AGR, y = science, label = country)) +
  stat_regline_equation(
    aes(x = avg_AGR, y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3.5, label.y = 320
  ) +
  stat_cor(
    aes(x = avg_AGR, y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 3.4, label.y = 320
  ) +
  ggsave("plots/agreeableness~science.png")
s3

#plot average conscientiousness vs average science per country
s4 <- ggplot(world) +
  geom_point(aes(x = avg_CSN, y = science, color = continent)) +
  coord_cartesian(xlim = c(3.1, 3.475)) +
  geom_smooth(aes(x = avg_CSN, y = science), method = "lm") +
  geom_text(aes(x = avg_CSN, y = science, label = country)) +
  stat_regline_equation(
    aes(x = avg_CSN, y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3.25, label.y = 320
  ) +
  stat_cor(
    aes(x = avg_CSN, y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 3.15, label.y = 320
  ) +
  ggsave("plots/conscientiousness~science.png")
s4

#plot average openness vs average science per country
s5 <- ggplot(world) +
  geom_point(aes(x = avg_OPN, y = science, color = continent)) +
  coord_cartesian(xlim = c(3.45, 4.15)) +
  geom_smooth(aes(x = avg_OPN, y = science), method = "lm") +
  geom_text(aes(x = avg_OPN, y = science, label = country)) +
  stat_regline_equation(
    aes(x = avg_OPN, y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3.7, label.y = 320
  ) +
  stat_cor(
    aes(x = avg_OPN, y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 3.5, label.y = 320
  ) +
  ggsave("plots/openness~science.png")
s5

#setup plot layout
grid.arrange(s1, s2, s3, s4, s5, nrow = 3)

#compare graphs ----------------------------------------------------------------
#extroversion
grid.arrange(m1, r1, s1, nrow = 3)

#neuroticism
grid.arrange(m2, r2, s2, nrow = 3)

#agreeableness
grid.arrange(m3, r3, s3, nrow = 3)

#conscientiousness
grid.arrange(m4, r4, s4, nrow = 3)

#openness
grid.arrange(m5, r5, s5, nrow = 3)

# ==============================================================================
#top graphs
grid.arrange(m1, r1, r3, nrow = 3)
