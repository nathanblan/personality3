#time plots

#plot time ---------------------------------------------------------------------
#plot average extroversion-time vs average extroversion per country
t1 <- ggplot(world) +
  geom_point(aes(x = med_EXT, y = log(avg_EXT_E), color = continent)) +
  geom_smooth(aes(x = med_EXT, y = log(avg_EXT_E)), method = "lm") +
  geom_text(aes(x = med_EXT, y = log(avg_EXT_E), label = country)) +
  stat_regline_equation(
    aes(x = avg_EXT, y = log(avg_EXT_E), 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 5
  ) +
  stat_cor(
    aes(x = avg_EXT, y = log(avg_EXT_E),
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 5
  ) +
  ggsave("plots/extraversion-time~extroversion.png")
t1
#plot average neuroticism-time vs average neuroticism per country
t2 <- ggplot(world) +
  geom_point(aes(x = avg_EST, y = log(avg_EST_E), color = continent)) +
  geom_smooth(aes(x = avg_EST, y = log(avg_EST_E)), method = "lm") +
  geom_text(aes(x = avg_EST, y = log(avg_EST_E), label = country)) +
  stat_regline_equation(
    aes(x = avg_EST, y = log(avg_EST_E), 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 5
  ) +
  stat_cor(
    aes(x = avg_EST, y = log(avg_EST_E),
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 5
  ) +
  ggsave("plots/neuroticism-time~neuroticism.png")
t2
#plot average agreeableness-time vs average agreeableness per country
t3 <- ggplot(world) +
  geom_point(aes(x = avg_AGR, y = log(avg_AGR_E), color = continent)) +
  geom_smooth(aes(x = avg_AGR, y = log(avg_AGR_E)), method = "lm") +
  geom_text(aes(x = avg_AGR, y = log(avg_AGR_E), label = country)) +
  stat_regline_equation(
    aes(x = avg_AGR, y = log(avg_AGR_E), 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 5
  ) +
  stat_cor(
    aes(x = avg_AGR, y = log(avg_AGR_E),
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 5
  ) +
  ggsave("plots/agreeableness-time~agreeableness.png")
t3
#plot average conscientiousness-time vs average conscientiousness per country
t4 <- ggplot(world) +
  geom_point(aes(x = avg_CSN, y = log(avg_CSN_E), color = continent)) +
  geom_smooth(aes(x = avg_CSN, y = log(avg_CSN_E)), method = "lm") +
  geom_text(aes(x = avg_CSN, y = log(avg_CSN_E), label = country)) +
  stat_regline_equation(
    aes(x = avg_CSN, y = log(avg_CSN_E), 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 5
  ) +
  stat_cor(
    aes(x = avg_CSN, y = log(avg_CSN_E),
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 5
  ) +
  ggsave("plots/conscientiousness-time~conscientiousness.png")
t4
#plot average openness-time vs average openness per country
t5 <- ggplot(world) +
  geom_point(aes(x = avg_OPN, y = log(avg_OPN_E), color = continent)) +
  geom_smooth(aes(x = avg_OPN, y = log(avg_OPN_E)), method = "lm") +
  geom_text(aes(x = avg_OPN, y = log(avg_OPN_E), label = country)) +
  stat_regline_equation(
    aes(x = avg_OPN, y = log(avg_OPN_E), 
        label =  paste(..eq.label.., sep = "~~~~")),
    label.x = 3, label.y = 5
  ) +
  stat_cor(
    aes(x = avg_OPN, y = log(avg_OPN_E),
        label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1, label.y = 5
  ) +
  ggsave("plots/openness-time~openness.png")
t5
#setup plot layout
grid.arrange(t1, t2, t3, t4, t5, nrow = 3)

