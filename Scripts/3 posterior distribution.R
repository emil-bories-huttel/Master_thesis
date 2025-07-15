load("data/cleaned_data.RData") 

library(tidyverse)
library(patchwork)


df_post <- df |> 
  mutate(p1i_post = p1i_post / 100,
         p2i_post = p2i_post / 100) |> 
  filter(post_correct == 0 & prior_correct == 0) ## filter respondents with more than one comprehension error and/or incorrect posterior

### global 
mu_overall <- mean(df_post$y1i_post, na.rm = TRUE)
sigma_overall <- (mean(df_post$y3i_post) - mean(df_post$y2i_post)) / (qnorm(1 - mean(df_post$p2i_post)) - qnorm(mean(df_post$p1i_post))) 

# Individual distributions
params_df <- df_post %>%
  rowwise() %>%
  mutate(
    mu_est = y1i_post,
    sd_est = max(
      (y3i_post - y2i_post) / (qnorm(1 - p2i_post) - qnorm(p1i_post)),
      1 # Impose SD of minimum 1 for plot readability
    )
  ) %>%
  ungroup() %>%
  select(mean = mu_est, sd = sd_est)  

df_post <- df_post |> 
  bind_cols(params_df) |> 
  select(-y1i_post, -y2i_post, -y3i_post, -p1i_post, -p2i_post) 
  # round to whole digits 
  #mutate(sd = round(sd, 0)) |>
  #mutate(sd = round(sd * 2) / 2) |>  # round sds to whole or half digits 
  # if above 10, set to 10 
  #mutate(sd = ifelse(sd > 10, 10, sd))

params_df <- params_df |> 
  filter(sd <= 40)


# Create a sequence of x values for plotting
x_values <- seq(-50, 50, length.out = 500)

# Create a data frame for plotting
plot_data <- data.frame(x = rep(x_values, nrow(params_df)),
                        mean = rep(params_df$mean, each = length(x_values)),
                        sd = rep(params_df$sd, each = length(x_values)),
                        respondent = rep(rownames(params_df), each = length(x_values)), 
                        o_mean = rep(mu_overall),
                        o_sigma = rep(sigma_overall)
)

# Calculate the density values for each respondent
plot_data$density <- dnorm(plot_data$x, mean = plot_data$mean, sd = plot_data$sd)
plot_data$o_density <- dnorm(plot_data$x, mean = plot_data$o_mean, sd = plot_data$o_sigma)

mu_sd_temp <- bquote(atop(mu == .(round(mu_overall, 2)), 
                          sigma == .(round(sigma_overall, 2))))

# Plot using ggplot2
gg.post <- ggplot(plot_data, aes(x = x, y = density, group = respondent)) +
  geom_line(alpha = .05, color = "#B06767", lwd = .5) +
  geom_line(aes(y = o_density), color = "#911C1C", linewidth = .3, alpha = .5) + 
  labs(x = "Expected price change", 
       y = "") +
  theme(legend.position = "none") + 
  guides(x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE)) + 
  annotate("text", x = 35, y = .35, 
           label = mu_sd_temp, size = 2.5) +
  facet_wrap(~"Fitted posterior") + 
  xlim(-50, 50) +
  scale_y_continuous(position = "right")

# compute probability of being above 0 
pnorm(0, mean = mu_overall, sd = sigma_overall, lower.tail = FALSE)

# density between -1 sd and 1 sd 
pnorm((mu_overall+sigma_overall), mean = mu_overall, sd = sigma_overall) - pnorm((mu_overall-sigma_overall), mean = mu_overall, sd = sigma_overall)



## show number of NAs (error check)
sum(is.na(params_df$sd))

## negative sigma? (error check, sigma can never be 0 or negative)
params_df |> 
  filter(sd < 0) |> 
  nrow()


### 2d density plot 
ggplot(params_df, aes(x = mean, y = sd)) + 
  geom_point(shape = 1, color = "#B06767") + 
  labs(x = "Mean", y = "SD") + 
  theme(legend.position = "none") + 
  guides(x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE)) + 
  geom_density_2d(color = "#911C1C") +
  ylim(0, 8) +
  xlim(-40, 40) 
#geom_vline(xintercept = mu_overall, linetype = "dashed", color = "green") + 
#geom_hline(yintercept = sigma_overall, linetype = "dashed", color = "green") + 
#annotate("text", x = 60, y = 10, label = paste("Mean =", round(mu_overall, 2), "\n", "SD =", round(sigma_overall, 2)), size = 3, color = "black", hjust = 0)

wrap_plots(gg.prior, gg.post, nrow = 1) + 
  plot_layout(axis_titles = "collect") 

