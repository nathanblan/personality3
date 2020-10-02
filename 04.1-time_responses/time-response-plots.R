#time plots

#plot time vs raw responses ----------------------------------------------------
#plot average extroversion-time vs average extroversion per country
tt1 <- ggplot(world) +
  geom_point(aes(x = avg_EXT, y = log(avg_EXT_E), color = continent)) +
  geom_smooth(aes(x = avg_EXT, y = log(avg_EXT_E)), method = "lm") +
  geom_text(aes(x = avg_EXT, y = log(avg_EXT_E), label = country)) +
  stat_regline_equation(
    aes(x = avg_EXT, y = log(avg_EXT_E), 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 8
  ) +
  stat_cor(
    aes(x = avg_EXT, y = log(avg_EXT_E),
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 2, label.y = 8
  ) +
  ggsave("plots/extraversion-time~extroversion.png")
tt1
#plot average neuroticism-time vs average neuroticism per country
tt2 <- ggplot(world) +
  geom_point(aes(x = avg_EST, y = log(avg_EST_E), color = continent)) +
  geom_smooth(aes(x = avg_EST, y = log(avg_EST_E)), method = "lm") +
  geom_text(aes(x = avg_EST, y = log(avg_EST_E), label = country)) +
  stat_regline_equation(
    aes(x = avg_EST, y = log(avg_EST_E), 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 7.5
  ) +
  stat_cor(
    aes(x = avg_EST, y = log(avg_EST_E),
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 2, label.y = 7.5
  ) +
  ggsave("plots/neuroticism-time~neuroticism.png")
tt2
#plot average agreeableness-time vs average agreeableness per country
tt3 <- ggplot(world) +
  geom_point(aes(x = avg_EST, y = log(avg_AGR_E), color = continent)) +
  geom_smooth(aes(x = avg_EST, y = log(avg_AGR_E)), method = "lm") +
  geom_text(aes(x = avg_EST, y = log(avg_AGR_E), label = country)) +
  stat_regline_equation(
    aes(x = avg_EST, y = log(avg_AGR_E), 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 4, label.y = 8
  ) +
  stat_cor(
    aes(x = avg_EST, y = log(avg_AGR_E),
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 3.5, label.y = 8
  ) +
  ggsave("plots/agreeableness-time~agreeableness.png")
tt3
#plot average conscientiousness-time vs average conscientiousness per country
tt4 <- ggplot(world) +
  geom_point(aes(x = avg_CSN, y = log(avg_CSN_E), color = continent)) +
  geom_smooth(aes(x = avg_CSN, y = log(avg_CSN_E)), method = "lm") +
  geom_text(aes(x = avg_CSN, y = log(avg_CSN_E), label = country)) +
  stat_regline_equation(
    aes(x = avg_CSN, y = log(avg_CSN_E), 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 8
  ) +
  stat_cor(
    aes(x = avg_CSN, y = log(avg_CSN_E),
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1.5, label.y = 8
  ) +
  ggsave("plots/conscientiousness-time~conscientiousness.png")
tt4
#plot average openness-time vs average openness per country
tt5 <- ggplot(world) +
  geom_point(aes(x = avg_OPN, y = log(avg_OPN_E), color = continent)) +
  geom_smooth(aes(x = avg_OPN, y = log(avg_OPN_E)), method = "lm") +
  geom_text(aes(x = avg_OPN, y = log(avg_OPN_E), label = country)) +
  stat_regline_equation(
    aes(x = avg_OPN, y = log(avg_OPN_E), 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 4, label.y = 8
  ) +
  stat_cor(
    aes(x = avg_OPN, y = log(avg_OPN_E),
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 3.5, label.y = 8
  ) +
  ggsave("plots/openness-time~openness.png")
tt5
#setup plot layout
grid.arrange(tt1, tt2, tt3, tt4, tt5, nrow = 3)

##plot time vs pisa ============================================================
#plot math ---------------------------------------------------------------------
#plot average extroversion time vs average math per country
tm1 <- ggplot(world) +
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
tm1

#plot average neuroticism time vs average math per country
tm2 <- ggplot(world) +
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
tm2

#plot average agreeableness time vs average math per country
tm3 <- ggplot(world) +
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
tm3

#plot average conscientiousness time vs average math per country
tm4 <- ggplot(world) +
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
tm4

#plot average openness time vs average math per country
tm5 <- ggplot(world) +
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
tm5

#setup plot layout
grid.arrange(tm1, tm2, tm3, tm4, tm5, nrow = 3)

#plot reading ------------------------------------------------------------------
#plot average extroversion vs average reading per country
tr1 <- ggplot(world) +
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
tr1

#plot average neuroticism vs average reading per country
tr2 <- ggplot(world) +
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
tr2

#plot average agreeableness vs average reading per country
tr3 <- ggplot(world) +
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
tr3

#plot average conscientiousness vs average reading per country
tr4 <- ggplot(world) +
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
tr4

#plot average openness vs average reading per country
tr5 <- ggplot(world) +
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
tr5

#setup plot layout
grid.arrange(tr1, tr2, tr3, tr4, tr5, nrow = 3)

#plot science ------------------------------------------------------------------
#plot average extroversion vs average science per country
ts1 <- ggplot(world) +
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
ts1

#plot average neuroticism vs average science per country
ts2 <- ggplot(world) +
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
ts2

#plot average agreeableness vs average science per country
ts3 <- ggplot(world) +
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
ts3

#plot average conscientiousness vs average science per country
ts4 <- ggplot(world) +
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
ts4

#plot average openness vs average science per country
ts5 <- ggplot(world) +
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
ts5

#compare graphs ----------------------------------------------------------------
#extroversion
grid.arrange(tm1, tr1, ts1, nrow = 3)
 
#neuroticism
grid.arrange(tm2, tr2, ts2, nrow = 3)

#agreeableness
grid.arrange(tm3, tr3, ts3, nrow = 3)

#conscientiousness
grid.arrange(tm4, tr4, ts4, nrow = 3)

#openness
grid.arrange(tm5, tr5, ts5, nrow = 3)

# ==============================================================================
#top graphs
grid.arrange(tr2, ts2, tr5, nrow = 3)
