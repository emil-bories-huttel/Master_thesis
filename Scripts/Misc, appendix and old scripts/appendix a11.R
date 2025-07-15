

df_post <- df_post %>%
  mutate(
    p_gt_0 = 1 - pnorm(0, mean = mean, sd = sd),
    category = ceiling(p_gt_0 * 20),
    category = if_else(category == 0, 1L, category)  # fix for p = 0 edge case
  )


df_post |> 
  group_by(category) |>
  # count 
  summarise(
    n = n())


stage1_1 <- bf(update ~ 1 + treat) 
stage2_2 <- bf(update_dev ~ 1 + update + (update | category))

eq10_sd3 <- brm(data = df_post, 
                family = gaussian,
                stage1_1 + stage2_2 + set_rescor(TRUE),
                prior = c(
                  # first stage
                  prior(normal(0, 11), class = Intercept, resp = update),
                  prior(normal(0, 11), class = b, resp = update),
                  prior(exponential(11), class = sigma, resp = update),
                  
                  # second stage 
                  prior(normal(0, 0.67), class = Intercept, resp = updatedev),
                  prior(normal(0, 0.67), class = b, resp = updatedev),
                  prior(exponential(0.67), class = sigma, resp = updatedev),
                  
                  # rho
                  prior(lkj(2), class = rescor),
                  
                  # random effects 
                  prior(exponential(0.67), class = sd, resp = updatedev)),
                
                
                iter = 2000, 
                warmup = 1000, 
                chains = 4, 
                cores = 4,
                seed = 14)

posterior_sd <- eq10_sd3 %>%
  spread_draws(b_updatedev_update, r_category__updatedev[category, update]) %>%
  mutate(group_value = b_updatedev_update + r_category__updatedev)




posterior_sd %>%
  ggplot(aes(x = as.numeric(category), y = group_value)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  #geom_smooth(method = "loess", se = FALSE, color = "#911C1C", lwd = 0.5) +
  geom_line(aes(group = 1), stat = "summary", fun = median, color = "#911C1C", lwd = 0.5) +
  stat_pointinterval(lwd = .7, color = "#911C1C", alpha = .7, size = 3, shape = 21, fill = "white") + 
  scale_x_continuous(breaks = 1:20, 
                     labels = c(
                       "1"  = "0.00-0.05",
                       "2"  = "0.05-0.10",
                       "3"  = "0.10-0.15",
                       "4"  = "0.15-0.20",
                       "5"  = "0.20-0.25",
                       "6"  = "0.25-0.30",
                       "7"  = "0.30-0.35",
                       "8"  = "0.35-0.40",
                       "9"  = "0.40-0.45",
                       "10" = "0.45-0.50",
                       "11" = "0.50-0.55",
                       "12" = "0.55-0.60",
                       "13" = "0.60-0.65",
                       "14" = "0.65-0.70",
                       "15" = "0.70-0.75",
                       "16" = "0.75-0.80",
                       "17" = "0.80-0.85",
                       "18" = "0.85-0.90",
                       "19" = "0.90-0.95",
                       "20" = "0.95-1.00"
                     )) + 
  ylim(-0.2, 0.2) +
  labs(y = "Change in preferences", 
       x = "Respondents posterior probability of positive price change") + 
  theme(axis.text.x = element_text(angle = 45, hjust = .9))
 

