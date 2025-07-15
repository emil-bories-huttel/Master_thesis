credibility_dist <- df %>%
  count(credibility) %>%
  mutate(prop = n / sum(n)) 

posterior <- m2t %>%
  spread_draws(b_e_prior, r_credibility[credibility, term]) %>%
  filter(term %in% c("e_prior")) %>%
  mutate(group_value = case_when(
    term == "e_prior" ~ b_e_prior + r_credibility
  ))

posterior %>%
  filter(term == "e_prior") %>%
  ggplot(aes(x = as.numeric(credibility), y = group_value)) +
  geom_hline(yintercept = .85, linetype = "dotted", color = "grey40", lwd = .5) +
  geom_hline(yintercept = .65, color = "grey40", lwd = .5) +
  geom_hline(yintercept = .39, linetype = "dotted", color = "grey40", lwd = .5) +
  # Distribution as background bars (scaled to relative height)
  geom_bar(data = credibility_dist, aes(x = as.numeric(credibility), y = prop), 
           stat = "identity", fill = "#E6CBCB", width = 0.4, alpha = 0.3, inherit.aes = FALSE) +
  geom_smooth(method = "loess", se = FALSE, color = "#E6CBCB", lwd = .5) +
  geom_line(aes(group = 1), stat = "summary", fun = median, color = "#911C1C", lwd = .5) + 
  stat_pointinterval(lwd = .7, color = "#911C1C", alpha = .7, size = 3, shape = 21, fill = "white") +
  #stat_slab() + 
  
  # Y-axis labels for effect estimate
  labs(y = expression(pi[s]), x = expression("Credibility Level " ~(sigma[x]^2 ~"proxy"))) +
  guides(y = guide_axis(minor.ticks = TRUE)) +
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  scale_y_continuous(expand = c(0.003, 0), limits = c(-.01, 1.1)) 







sd_dist <- df_prior %>%
  count(sd) %>%
  mutate(prop = n / sum(n))  # normalize if you want relative height

posterior <- m2_2 %>%
  spread_draws(b_e_prior, r_sd[sd, term]) %>%
  filter(term %in% c("e_prior")) %>%
  mutate(group_value = case_when(
    term == "e_prior" ~ b_e_prior + r_sd
  ))

posterior %>%
  filter(term == "e_prior") %>%
  ggplot(aes(x = as.numeric(sd), y = group_value)) +
  geom_hline(yintercept = .82, linetype = "dotted", color = "grey40", lwd = .5) +
  geom_hline(yintercept = .63, color = "grey40", lwd = .5) +
  geom_hline(yintercept = .40, linetype = "dotted", color = "grey40", lwd = .5) +
  # Distribution as background bars (scaled to relative height)
  geom_bar(data = sd_dist, aes(x = as.numeric(sd), y = prop), 
           stat = "identity", fill = "#E6CBCB", width = 0.4, alpha = 0.3, inherit.aes = FALSE) +
  geom_smooth(method = "loess", se = FALSE, color = "#E6CBCB", lwd = .5) +
  geom_line(aes(group = 1), stat = "summary", fun = median, color = "#911C1C", lwd = .5) + 
  stat_pointinterval(lwd = .7, color = "#911C1C", alpha = .7, size = 3, shape = 21, fill = "white") +
  #stat_slab() + 
  
  # Y-axis labels for effect estimate
  labs(y = expression(pi[s]), x = expression(sigma[0]^2)) +
  guides(y = guide_axis(minor.ticks = TRUE)) +
  scale_x_continuous(breaks = seq(1, 10, 1), 
                     labels = c(
                       "1" = "1",
                       "2" = "2",
                       "3" = "3",
                       "4" = "4",
                       "5" = "5",
                       "6" = "6",
                       "7" = "7",
                       "8" = "8",
                       "9" = "9",
                       "10" = "10+")) + 
  scale_y_continuous(expand = c(0.003, 0), limits = c(-.01, 1.1))







