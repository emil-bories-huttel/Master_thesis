library(brms)

df_long1 <- df |> 
  mutate(id = row_number()) |> 
  select(treat, y1i_prior, y1i_post, e_prior, pro_dev_prior, pro_dev_post, id) |> 
  pivot_longer(cols = c(y1i_prior, y1i_post), names_to = "time", values_to = "y") |> 
  mutate(time = case_when(
    time == "y1i_prior" ~ 0,
    time == "y1i_post" ~ 1
  )) 

df_long2 <- df |> 
  mutate(id = row_number()) |> 
  select(pro_dev_prior, pro_dev_post, id, treat) |> 
  pivot_longer(cols = c(pro_dev_prior, pro_dev_post), names_to = "time", values_to = "y_dev") |> 
  mutate(time = case_when(
    time == "pro_dev_prior" ~ 0,
    time == "pro_dev_post" ~ 1
  )) 


########
# EQ ? #
########

eq <- brm(
  y ~ time,
  data = df_long1,
  family = gaussian(),
  prior = c(
    prior(normal(0, 11.9), class = "Intercept"),
    prior(normal(0, 11.9), class = "b"),
    prior(exponential(11.9), class = "sigma")
  ),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4) 

posterior_draws <- eq %>%
  spread_draws(b_time, Intercept) |>
  mutate(
    mean_intercept = mean(Intercept),
    mean_slope = mean(b_time)
  ) |> 
  ## randomly draw 100 samples
  sample_n(200) 

mean_df <- posterior_draws |> 
  select(mean_intercept, mean_slope) |>
  group_by(mean_intercept, mean_slope) |>
  summarise(mean_intercept = mean(mean_intercept), mean_slope = mean(mean_slope))

df_long1 |> 
  ggplot(aes(x = time, y = y)) +
  geom_point(color = NA) + 
  geom_abline(aes(intercept = Intercept, slope = b_time), data = posterior_draws, color = "#B06767", alpha = .1) +
  geom_abline(aes(intercept = mean_intercept , slope = mean_slope), data = posterior_draws, color = "green", alpha = .6) + 
  ylim(-5, 2) + 
  # notice that the intercept is centered
  geom_vline(xintercept = -.5, linetype = "dotted", color = "black") +
  geom_vline(xintercept = .5, linetype = "dotted", color = "black") +
  scale_x_continuous(limit = c(-.5,.5), breaks = c(-.5, .5), labels = c("pre", "post"))

df_long1 |> 
  ggplot(aes(x = time, y = y)) +
  geom_point(color = NA) + 
  # Use geom_segment to specify the start and end points of the lines
  geom_segment(
    aes(x = -0.5, xend = 0.5, y = Intercept + b_time * -0.5, yend = Intercept + b_time * 0.5), 
    data = posterior_draws, 
    color = "#B06767", alpha = 0.1
  ) + 
  geom_segment(
    aes(x = -0.5, xend = 0.5, y = mean_intercept + mean_slope * -0.5, yend = mean_intercept + mean_slope * 0.5), 
    data = mean_df, 
    color = "#911C1C", lwd = .3
  ) +
  ylim(-5, 2) + 
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "black") +
  geom_vline(xintercept = 0.5, linetype = "dotted", color = "black") +
  scale_x_continuous(limit = c(-0.6, 0.6), breaks = c(-0.5, 0.5), labels = c("pre", "post")) 

    
