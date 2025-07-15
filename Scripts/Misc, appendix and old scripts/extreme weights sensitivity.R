

df_no <- df |>
  mutate(weight = update/e_prior) |> 
  # only within -6 ; 6 
  filter(weight >= -6 & weight <= 6) 
 

eq7_no <- brm(data = df_no, 
           family = gaussian, 
           y1i_post ~ 0 + y1i_prior + signal,
           prior = c(prior(beta(2, 2), lb = 0, ub = 1, class = b),
                     prior(exponential(11.24), class = sigma)), #exp(sd(df$y1i_post)
           
           warmup = 1000,
           iter = 2000, 
           chains = 4,
           cores = 4,
           seed = 6582) 


eq6_4_no <- brm(
  update ~ treat,
  data = df_no,
  family = gaussian(),
  prior = c(
    prior(normal(0, 12.07), class = "Intercept"),
    prior(normal(0, 12.07), class = "b"),
    prior(exponential(12.07), class = "sigma")
  ),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4, 
  seed = 6582) 

eq6_4_no
eq7_no

eq7_sum_draws_no <- eq7_no %>%
  spread_draws(b_y1i_prior, b_signal) |> 
  mutate(
    sum = b_y1i_prior + b_signal 
  ) |> 
  pivot_longer(cols = c(sum), names_to = "coef", values_to = "y") |> 
  mutate(coef = case_when(
    coef == "sum" ~ "Sum")) 

# sum summary 66 and 95 
eq7_sum_draws_no |> 
  summarise(
    median = median(y),
    Q2.5 = quantile(y, 0.025),
    Q33 = quantile(y, 0.17),   # Lower bound approx for 66% CI
    Q67 = quantile(y, 0.83),   # Upper bound approx for 66% CI
    Q97.5 = quantile(y, 0.975)
  ) 

eq7_draws_no <- eq7_no %>%
  spread_draws(b_y1i_prior, b_signal) |> 
  # long 
  pivot_longer(cols = c(b_y1i_prior, b_signal), names_to = "coef", values_to = "y") |>
  mutate(coef = case_when(
    coef == "b_y1i_prior" ~ "Prior",
    coef == "b_signal" ~ "Signal"
  ))

eq7.gg1_no <- eq7_draws_no |> 
  ggplot(aes(x = coef, y = y)) +
  stat_pointinterval(lwd = .7, color = "#911C1C", alpha = .7, size = 3, shape = 21, fill = "white") +
  #geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.2, fill = "#B06767", color = "#911C1C") + 
  labs(
    x = "",
    y = "") + 
  scale_y_continuous(position = "right", limits = c(0,1)) + 
  coord_flip() + 
  guides(x = guide_axis(minor.ticks = TRUE)) +
  theme(panel.grid.major.y = element_line(color = "grey50", linetype = "dotted", size = 0.5))


eq7.gg2_no <- eq7_sum_draws_no |>
  ggplot(aes(x = coef, y = y)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey30") +  
  stat_pointinterval(lwd = .7, color = "#911C1C", alpha = .7, size = 3, shape = 21, fill = "white") +
  #geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.2, fill = "#B06767", color = "#911C1C") + 
  labs(
    x = "",
    y = "") + 
  scale_y_continuous(limits = c(.5,1.5)) + 
  coord_flip() + 
  guides(x = guide_axis(minor.ticks = TRUE)) + 
  theme(panel.grid.major.y = element_line(color = "grey50", linetype = "dotted", size = 0.5))

wrap_plots(eq7.gg1_no, eq7.gg2_no, nrow = 2) +
  plot_layout(heights = c(2.5, 1)) 










# predict y at treat = 1 
newdata_eq6_4_no <- data.frame(treat = 1)

fitted(eq6_4_no, newdata = newdata_eq6_4_no, probs = c(0.025, 0.975))

intercept4_test_no <- eq6_4_no |> 
  spread_draws(b_Intercept, b_treat) |> 
  mutate(
    fitted_treat1 = b_Intercept + b_treat,
    ratio = fitted_treat1 / b_Intercept
  )

# Compute probability that fitted at treat = 1 is between 3.5 * intercept or 4.5 * intercept 
intercept4_test_no |> 
  summarise(prob_between = mean(ratio > 3.5 & ratio < 4.5))
