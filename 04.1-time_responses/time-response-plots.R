#time plots

#plot time ---------------------------------------------------------------------
#plot average extroversion-time vs average extroversion per country
t1 <- ggplot(world) +
  geom_point(aes(x = avg_EXT_E, y = avg_EXT, color = continent)) +
  geom_smooth(aes(x = avg_EXT_E, y = avg_EXT), method = "lm") +
  geom_text(aes(x = avg_EXT_E, y = avg_EXT, label = country)) +
  ggsave("plots/extraversion-time~extroversion.png")

#plot average neuroticism-time vs average neuroticism per country
t2 <- ggplot(world) +
  geom_point(aes(x = avg_EST_E, y = avg_EST, color = continent)) +
  geom_smooth(aes(x = avg_EST_E, y = avg_EST), method = "lm") +
  geom_text(aes(x = avg_EST_E, y = avg_EST, label = country)) +
  ggsave("plots/neuroticism-time~neuroticism.png")

#plot average agreeableness-time vs average agreeableness per country
t3 <- ggplot(world) +
  geom_point(aes(x = avg_AGR_E, y = avg_AGR, color = continent)) +
  geom_smooth(aes(x = avg_AGR_E, y = avg_AGR), method = "lm") +
  geom_text(aes(x = avg_AGR_E, y = avg_AGR, label = country)) +
  ggsave("plots/agreeableness-time~agreeableness.png")

#plot average conscientiousness-time vs average conscientiousness per country
t4 <- ggplot(world) +
  geom_point(aes(x = avg_CSN_E, y = avg_CSN, color = continent)) +
  geom_smooth(aes(x = avg_CSN_E, y = avg_CSN), method = "lm") +
  geom_text(aes(x = avg_CSN_E, y = avg_CSN, label = country)) +
  ggsave("plots/conscientiousness-time~conscientiousness.png")

#plot average openness-time vs average openness per country
t5 <- ggplot(world) +
  geom_point(aes(x = avg_OPN_E, y = avg_OPN, color = continent)) +
  geom_smooth(aes(x = avg_OPN_E, y = avg_OPN), method = "lm") +
  geom_text(aes(x = avg_OPN_E, y = avg_OPN, label = country)) +
  ggsave("plots/openness-time~openness.png")

#setup plot layout
grid.arrange(t1, t2, t3, t4, t5, nrow = 3)
