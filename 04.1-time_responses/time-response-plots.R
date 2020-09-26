#time plots

#plot time ---------------------------------------------------------------------
#plot average extroversion-time vs average extroversion per country
t1 <- ggplot(world) +
  geom_point(aes(x = log(avg_EXT_E), y = log(avg_EXT_E), color = continent)) +
  geom_smooth(aes(x = log(avg_EXT_E), y = log(avg_EXT_E)), method = "lm") +
  geom_text(aes(x = log(avg_EXT_E), y = log(avg_EXT_E), label = country)) +
  stat_regline_equation(
    aes(x = log(avg_EXT_E), y = log(avg_EXT_E), 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 8
  ) +
  stat_cor(
    aes(x = log(avg_EXT_E), y = log(avg_EXT_E),
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 2, label.y = 8
  ) +
  ggsave("plots/extraversion-time~extroversion.png")
t1
#plot average neuroticism-time vs average neuroticism per country
t2 <- ggplot(world) +
  geom_point(aes(x = log(avg_EST_E), y = log(avg_EST_E), color = continent)) +
  geom_smooth(aes(x = log(avg_EST_E), y = log(avg_EST_E)), method = "lm") +
  geom_text(aes(x = log(avg_EST_E), y = log(avg_EST_E), label = country)) +
  stat_regline_equation(
    aes(x = log(avg_EST_E), y = log(avg_EST_E), 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 7.5
  ) +
  stat_cor(
    aes(x = log(avg_EST_E), y = log(avg_EST_E),
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 2, label.y = 7.5
  ) +
  ggsave("plots/neuroticism-time~neuroticism.png")
t2
#plot average agreeableness-time vs average agreeableness per country
t3 <- ggplot(world) +
  geom_point(aes(x = log(avg_AGR_E), y = log(avg_AGR_E), color = continent)) +
  geom_smooth(aes(x = log(avg_AGR_E), y = log(avg_AGR_E)), method = "lm") +
  geom_text(aes(x = log(avg_AGR_E), y = log(avg_AGR_E), label = country)) +
  stat_regline_equation(
    aes(x = log(avg_AGR_E), y = log(avg_AGR_E), 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 4, label.y = 8
  ) +
  stat_cor(
    aes(x = log(avg_AGR_E), y = log(avg_AGR_E),
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 3.5, label.y = 8
  ) +
  ggsave("plots/agreeableness-time~agreeableness.png")
t3
#plot average conscientiousness-time vs average conscientiousness per country
t4 <- ggplot(world) +
  geom_point(aes(x = log(avg_CSN_E), y = log(avg_CSN_E), color = continent)) +
  geom_smooth(aes(x = log(avg_CSN_E), y = log(avg_CSN_E)), method = "lm") +
  geom_text(aes(x = log(avg_CSN_E), y = log(avg_CSN_E), label = country)) +
  stat_regline_equation(
    aes(x = log(avg_CSN_E), y = log(avg_CSN_E), 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 8
  ) +
  stat_cor(
    aes(x = log(avg_CSN_E), y = log(avg_CSN_E),
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1.5, label.y = 8
  ) +
  ggsave("plots/conscientiousness-time~conscientiousness.png")
t4
#plot average openness-time vs average openness per country
t5 <- ggplot(world) +
  geom_point(aes(x = log(avg_OPN_E), y = log(avg_OPN_E), color = continent)) +
  geom_smooth(aes(x = log(avg_OPN_E), y = log(avg_OPN_E)), method = "lm") +
  geom_text(aes(x = log(avg_OPN_E), y = log(avg_OPN_E), label = country)) +
  stat_regline_equation(
    aes(x = log(avg_OPN_E), y = log(avg_OPN_E), 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 4, label.y = 8
  ) +
  stat_cor(
    aes(x = log(avg_OPN_E), y = log(avg_OPN_E),
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 3.5, label.y = 8
  ) +
  ggsave("plots/openness-time~openness.png")
t5
#setup plot layout
grid.arrange(t1, t2, t3, t4, t5, nrow = 3)

##plot time vs pisa ============================================================
#plot math ---------------------------------------------------------------------
#plot average extroversion time vs average math per country
m1 <- ggplot(world) +
  geom_point(aes(x = log(avg_EXT_E), y = math, color = continent)) +
  coord_cartesian(xlim = c(8.55, 9.25)) +
  geom_smooth(aes(x = log(avg_EXT_E), y = math), method = "lm") +
  geom_text(aes(x = log(avg_EXT_E), y = math, label = country)) +
  stat_regline_equation(
    aes(x = log(avg_EXT_E), y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 9, label.y = 320
  ) +
  stat_cor(
    aes(x = log(avg_EXT_E), y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 8.6, label.y = 320
  ) +
  ggsave("plots/extraversion~math.png")
m1

#plot average neuroticism time vs average math per country
m2 <- ggplot(world) +
  geom_point(aes(x = log(avg_EST_E), y = math, color = continent)) +
  coord_cartesian(xlim = c(8.3, 9.05)) +
  geom_smooth(aes(x = log(avg_EST_E), y = math), method = "lm") +
  geom_text(aes(x = log(avg_EST_E), y = math, label = country)) +
  stat_regline_equation(
    aes(x = log(avg_EST_E), y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 8.6, label.y = 320
  ) +
  stat_cor(
    aes(x = log(avg_EST_E), y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 8.4, label.y = 320
  ) +
  ggsave("plots/neuroticism~math.png")
m2

#plot average agreeableness time vs average math per country
m3 <- ggplot(world) +
  geom_point(aes(x = log(avg_AGR_E), y = math, color = continent)) +
  coord_cartesian(xlim = c(8.4, 9.1)) +
  geom_smooth(aes(x = log(avg_AGR_E), y = math), method = "lm") +
  geom_text(aes(x = log(avg_AGR_E), y = math, label = country)) +
  stat_regline_equation(
    aes(x = log(avg_AGR_E), y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 8.6, label.y = 320
  ) +
  stat_cor(
    aes(x = log(avg_AGR_E), y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 8.4, label.y = 320
  ) +
  ggsave("plots/agreeableness~math.png")
m3

#plot average conscientiousness time vs average math per country
m4 <- ggplot(world) +
  geom_point(aes(x = log(avg_CSN_E), y = math, color = continent)) +
  coord_cartesian(xlim = c(8.45, 9.2)) +
  geom_smooth(aes(x = log(avg_CSN_E), y = math), method = "lm") +
  geom_text(aes(x = log(avg_CSN_E), y = math, label = country)) +
  stat_regline_equation(
    aes(x = log(avg_CSN_E), y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 8.7, label.y = 320
  ) +
  stat_cor(
    aes(x = log(avg_CSN_E), y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 8.5, label.y = 320
  ) +
  ggsave("plots/conscientiousness~math.png")
m4

#plot average openness time vs average math per country
m5 <- ggplot(world) +
  geom_point(aes(x = log(avg_OPN_E), y = math, color = continent)) +
  coord_cartesian(xlim = c(8.3, 8.95)) +
  geom_smooth(aes(x = log(avg_OPN_E), y = math), method = "lm") +
  geom_text(aes(x = log(avg_OPN_E), y = math, label = country)) +
  stat_regline_equation(
    aes(x = log(avg_OPN_E), y = math, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 8.6, label.y = 320
  ) +
  stat_cor(
    aes(x = log(avg_OPN_E), y = math,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 8.4, label.y = 320
  ) +
  ggsave("plots/openness~math.png")
m5

#setup plot layout
grid.arrange(m1, m2, m3, m4, m5, nrow = 3)

#plot reading ------------------------------------------------------------------
#plot average extroversion vs average reading per country
r1 <- ggplot(world) +
  geom_point(aes(x = log(avg_EXT_E), y = reading, color = continent)) +
  coord_cartesian(xlim = c(8.55, 9.25)) +
  geom_smooth(aes(x = log(avg_EXT_E), y = reading), method = "lm") +
  geom_text(aes(x = log(avg_EXT_E), y = reading, label = country)) +
  stat_regline_equation(
    aes(x = log(avg_EXT_E), y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 8.8, label.y = 320
  ) +
  stat_cor(
    aes(x = log(avg_EXT_E), y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 8.6, label.y = 320
  ) +
  ggsave("plots/extraversion~reading.png")
r1

#plot average neuroticism vs average reading per country
r2 <- ggplot(world) +
  geom_point(aes(x = log(avg_EST_E), y = reading, color = continent)) +
  coord_cartesian(xlim = c(8.3, 9.05)) +
  geom_smooth(aes(x = log(avg_EST_E), y = reading), method = "lm") +
  geom_text(aes(x = log(avg_EST_E), y = reading, label = country)) +
  stat_regline_equation(
    aes(x = log(avg_EST_E), y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 8.6, label.y = 320
  ) +
  stat_cor(
    aes(x = log(avg_EST_E), y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 8.4, label.y = 320
  ) +
  ggsave("plots/neuroticism~reading.png")
r2

#plot average agreeableness vs average reading per country
r3 <- ggplot(world) +
  geom_point(aes(x = log(avg_AGR_E), y = reading, color = continent)) +
  coord_cartesian(xlim = c(8.4, 9.1)) +
  geom_smooth(aes(x = log(avg_AGR_E), y = reading), method = "lm") +
  geom_text(aes(x = log(avg_AGR_E), y = reading, label = country)) +
  stat_regline_equation(
    aes(x = log(avg_AGR_E), y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 8.7, label.y = 320
  ) +
  stat_cor(
    aes(x = log(avg_AGR_E), y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 8.5, label.y = 320
  ) +
  ggsave("plots/agreeableness~reading.png")
r3

#plot average conscientiousness vs average reading per country
r4 <- ggplot(world) +
  geom_point(aes(x = log(avg_CSN_E), y = reading, color = continent)) +
  coord_cartesian(xlim = c(8.45, 9.2)) +
  geom_smooth(aes(x = log(avg_CSN_E), y = reading), method = "lm") +
  geom_text(aes(x = log(avg_CSN_E), y = reading, label = country)) +
  stat_regline_equation(
    aes(x = log(avg_CSN_E), y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 8.7, label.y = 320
  ) +
  stat_cor(
    aes(x = log(avg_CSN_E), y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 8.5, label.y = 320
  ) +
  ggsave("plots/conscientiousness~reading.png")
r4

#plot average openness vs average reading per country
r5 <- ggplot(world) +
  geom_point(aes(x = log(avg_OPN_E), y = reading, color = continent)) +
  coord_cartesian(xlim = c(8.3, 8.95)) +
  geom_smooth(aes(x = log(avg_OPN_E), y = reading), method = "lm") +
  geom_text(aes(x = log(avg_OPN_E), y = reading, label = country)) +
  stat_regline_equation(
    aes(x = log(avg_OPN_E), y = reading, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 8.6, label.y = 320
  ) +
  stat_cor(
    aes(x = log(avg_OPN_E), y = reading,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 8.4, label.y = 320
  ) +
  ggsave("plots/openness~reading.png")
r5

#setup plot layout
grid.arrange(r1, r2, r3, r4, r5, nrow = 3)

#plot science ------------------------------------------------------------------
#plot average extroversion vs average science per country
s1 <- ggplot(world) +
  geom_point(aes(x = log(avg_EXT_E), y = science, color = continent)) +
  coord_cartesian(xlim = c(8.55, 9.25)) +
  geom_smooth(aes(x = log(avg_EXT_E), y = science), method = "lm") +
  geom_text(aes(x = log(avg_EXT_E), y = science, label = country)) +
  stat_regline_equation(
    aes(x = log(avg_EXT_E), y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 8.8, label.y = 320
  ) +
  stat_cor(
    aes(x = log(avg_EXT_E), y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 8.6, label.y = 320
  ) +
  ggsave("plots/extraversion~science.png")
s1

#plot average neuroticism vs average science per country
s2 <- ggplot(world) +
  geom_point(aes(x = log(avg_EST_E), y = science, color = continent)) +
  coord_cartesian(xlim = c(8.3, 9.05)) +
  geom_smooth(aes(x = log(avg_EST_E), y = science), method = "lm") +
  geom_text(aes(x = log(avg_EST_E), y = science, label = country)) +
  stat_regline_equation(
    aes(x = log(avg_EST_E), y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 8.6, label.y = 320
  ) +
  stat_cor(
    aes(x = log(avg_EST_E), y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 8.4, label.y = 320
  ) +
  ggsave("plots/neuroticism~science.png")
s2

#plot average agreeableness vs average science per country
s3 <- ggplot(world) +
  geom_point(aes(x = log(avg_AGR_E), y = science, color = continent)) +
  coord_cartesian(xlim = c(8.4, 9.1)) +
  geom_smooth(aes(x = log(avg_AGR_E), y = science), method = "lm") +
  geom_text(aes(x = log(avg_AGR_E), y = science, label = country)) +
  stat_regline_equation(
    aes(x = log(avg_AGR_E), y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 8.7, label.y = 320
  ) +
  stat_cor(
    aes(x = log(avg_AGR_E), y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 8.5, label.y = 320
  ) +
  ggsave("plots/agreeableness~science.png")
s3

#plot average conscientiousness vs average science per country
s4 <- ggplot(world) +
  geom_point(aes(x = log(avg_CSN_E), y = science, color = continent)) +
  coord_cartesian(xlim = c(8.45, 9.2)) +
  geom_smooth(aes(x = log(avg_CSN_E), y = science), method = "lm") +
  geom_text(aes(x = log(avg_CSN_E), y = science, label = country)) +
  stat_regline_equation(
    aes(x = log(avg_CSN_E), y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 8.7, label.y = 320
  ) +
  stat_cor(
    aes(x = log(avg_CSN_E), y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 8.5, label.y = 320
  ) +
  ggsave("plots/conscientiousness~science.png")
s4

#plot average openness vs average science per country
s5 <- ggplot(world) +
  geom_point(aes(x = log(avg_OPN_E), y = science, color = continent)) +
  coord_cartesian(xlim = c(8.3, 8.95)) +
  geom_smooth(aes(x = log(avg_OPN_E), y = science), method = "lm") +
  geom_text(aes(x = log(avg_OPN_E), y = science, label = country)) +
  stat_regline_equation(
    aes(x = log(avg_OPN_E), y = science, 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 8.6, label.y = 320
  ) +
  stat_cor(
    aes(x = log(avg_OPN_E), y = science,
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 8.4, label.y = 320
  ) +
  ggsave("plots/openness~science.png")
s5

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
