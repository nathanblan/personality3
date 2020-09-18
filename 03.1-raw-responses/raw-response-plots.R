#response plots
#p values smaller than 0.01 -> probably relationship
# import libraries -------------------------------------------------------------
#set up ggplot world
library("rnaturalearth")
library("rnaturalearthdata")
library("ggrepel")
library("ggpubr")

# plot world averages ----------------------------------------------------------
# extract world data and join with joint
world <- 
  ne_countries(scale = "medium", returnclass = "sf") %>% 
  rename(country = sovereignt) %>% 
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

#plot math ---------------------------------------------------------------------
#plot average extroversion vs average math per country
m1 <- ggplot(world) +
  geom_point(aes(x = avg_EXT, y = math, color = continent)) +
  geom_smooth(aes(x = avg_EXT, y = math), method = "lm") +
  geom_text(aes(x = avg_EXT, y = math, label = country)) +
  stat_regline_equation(
    aes(x = avg_EXT, y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 300
  ) +
  stat_cor(
    aes(x = avg_EXT, y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 300
  ) +
  ggsave("plots/extraversion~math.png")
m1
#plot average neuroticism vs average math per country
m2 <- ggplot(world) +
  geom_point(aes(x = avg_EST, y = math, color = continent)) +
  geom_smooth(aes(x = avg_EST, y = math), method = "lm") +
  geom_text(aes(x = avg_EST, y = math, label = country)) +
  stat_regline_equation(
    aes(x = avg_EST, y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 300
  ) +
  stat_cor(
    aes(x = avg_EST, y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 300
  ) +
  ggsave("plots/neuroticism~math.png")

#plot average agreeableness vs average math per country
m3 <- ggplot(world) +
  geom_point(aes(x = avg_AGR, y = math, color = continent)) +
  geom_smooth(aes(x = avg_AGR, y = math), method = "lm") +
  geom_text(aes(x = avg_AGR, y = math, label = country)) +
  stat_regline_equation(
    aes(x = avg_AGR, y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 300
  ) +
  stat_cor(
    aes(x = avg_AGR, y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 300
  ) +
  ggsave("plots/agreeableness~math.png")

#plot average conscientiousness vs average math per country
m4 <- ggplot(world) +
  geom_point(aes(x = avg_CSN, y = math, color = continent)) +
  geom_smooth(aes(x = avg_CSN, y = math), method = "lm") +
  geom_text(aes(x = avg_CSN, y = math, label = country)) +
  stat_regline_equation(
    aes(x = avg_CSN, y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 300
  ) +
  stat_cor(
    aes(x = avg_CSN, y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 300
  ) +
  ggsave("plots/conscientiousness~math.png")

#plot average openness vs average math per country
m5 <- ggplot(world) +
  geom_point(aes(x = avg_OPN, y = math, color = continent)) +
  geom_smooth(aes(x = avg_OPN, y = math), method = "lm") +
  geom_text(aes(x = avg_OPN, y = math, label = country)) +
  stat_regline_equation(
    aes(x = avg_OPN, y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 300
  ) +
  stat_cor(
    aes(x = avg_OPN, y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 300
  ) +
  ggsave("plots/openness~math.png")

#setup plot layout
grid.arrange(m1, m2, m3, m4, m5, nrow = 3)

#plot reading ------------------------------------------------------------------
#plot average extroversion vs average reading per country
r1 <- ggplot(world) +
  geom_point(aes(x = avg_EXT, y = reading, color = continent)) +
  geom_smooth(aes(x = avg_EXT, y = reading), method = "lm") +
  geom_text(aes(x = avg_EXT, y = reading, label = country)) +
  stat_regline_equation(
    aes(x = avg_EXT, y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 300
  ) +
  stat_cor(
    aes(x = avg_EXT, y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 300
  ) +
  ggsave("plots/extraversion~reading.png")
r1
#plot average neuroticism vs average reading per country
r2 <- ggplot(world) +
  geom_point(aes(x = avg_EST, y = reading, color = continent)) +
  geom_smooth(aes(x = avg_EST, y = reading), method = "lm") +
  geom_text(aes(x = avg_EST, y = reading, label = country)) +
  stat_regline_equation(
    aes(x = avg_EST, y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 300
  ) +
  stat_cor(
    aes(x = avg_EST, y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 300
  ) +
  ggsave("plots/neuroticism~reading.png")

#plot average agreeableness vs average reading per country
r3 <- ggplot(world) +
  geom_point(aes(x = avg_AGR, y = reading, color = continent)) +
  geom_smooth(aes(x = avg_AGR, y = reading), method = "lm") +
  geom_text(aes(x = avg_AGR, y = reading, label = country)) +
  stat_regline_equation(
    aes(x = avg_AGR, y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 300
  ) +
  stat_cor(
    aes(x = avg_AGR, y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 300
  ) +
  ggsave("plots/agreeableness~reading.png")

#plot average conscientiousness vs average reading per country
r4 <- ggplot(world) +
  geom_point(aes(x = avg_CSN, y = reading, color = continent)) +
  geom_smooth(aes(x = avg_CSN, y = reading), method = "lm") +
  geom_text(aes(x = avg_CSN, y = reading, label = country)) +
  stat_regline_equation(
    aes(x = avg_CSN, y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 300
  ) +
  stat_cor(
    aes(x = avg_CSN, y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 300
  ) +
  ggsave("plots/conscientiousness~reading.png")

#plot average openness vs average reading per country
r5 <- ggplot(world) +
  geom_point(aes(x = avg_OPN, y = reading, color = continent)) +
  geom_smooth(aes(x = avg_OPN, y = reading), method = "lm") +
  geom_text(aes(x = avg_OPN, y = reading, label = country)) +
  stat_regline_equation(
    aes(x = avg_OPN, y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 300
  ) +
  stat_cor(
    aes(x = avg_OPN, y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 300
  ) +
  ggsave("plots/openness~reading.png")

#setup plot layout
grid.arrange(r1, r2, r3, r4, r5, nrow = 3)

#plot science ------------------------------------------------------------------
#plot average extroversion vs average science per country
s1 <- ggplot(world) +
  geom_point(aes(x = avg_EXT, y = science, color = continent)) +
  geom_smooth(aes(x = avg_EXT, y = science), method = "lm") +
  geom_text(aes(x = avg_EXT, y = science, label = country)) +
  stat_regline_equation(
    aes(x = avg_EXT, y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 300
  ) +
  stat_cor(
    aes(x = avg_EXT, y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 300
  ) +
  ggsave("plots/extraversion~science.png")

#plot average neuroticism vs average science per country
s2 <- ggplot(world) +
  geom_point(aes(x = avg_EST, y = science, color = continent)) +
  geom_smooth(aes(x = avg_EST, y = science), method = "lm") +
  geom_text(aes(x = avg_EST, y = science, label = country)) +
  stat_regline_equation(
    aes(x = avg_EST, y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 300
  ) +
  stat_cor(
    aes(x = avg_EST, y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 300
  ) +
  ggsave("plots/neuroticism~science.png")

#plot average agreeableness vs average science per country
s3 <- ggplot(world) +
  geom_point(aes(x = avg_AGR, y = science, color = continent)) +
  geom_smooth(aes(x = avg_AGR, y = science), method = "lm") +
  geom_text(aes(x = avg_AGR, y = science, label = country)) +
  stat_regline_equation(
    aes(x = avg_AGR, y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 300
  ) +
  stat_cor(
    aes(x = avg_AGR, y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 300
  ) +
  ggsave("plots/agreeableness~science.png")

#plot average conscientiousness vs average science per country
s4 <- ggplot(world) +
  geom_point(aes(x = avg_CSN, y = science, color = continent)) +
  geom_smooth(aes(x = avg_CSN, y = science), method = "lm") +
  geom_text(aes(x = avg_CSN, y = science, label = country)) +
  stat_regline_equation(
    aes(x = avg_CSN, y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 300
  ) +
  stat_cor(
    aes(x = avg_CSN, y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 300
  ) +
  ggsave("plots/conscientiousness~science.png")

#plot average openness vs average science per country
s5 <- ggplot(world) +
  geom_point(aes(x = avg_OPN, y = science, color = continent)) +
  geom_smooth(aes(x = avg_OPN, y = science), method = "lm") +
  geom_text(aes(x = avg_OPN, y = science, label = country)) +
  stat_regline_equation(
    aes(x = avg_OPN, y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 300
  ) +
  stat_cor(
    aes(x = avg_OPN, y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 300
  ) +
  ggsave("plots/openness~science.png")

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

