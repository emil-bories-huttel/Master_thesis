## Load data and set theme 

load("data/cleaned_data.RData") 

library(tidyverse)
library(patchwork)


custom_theme <- theme_bw() + 
  theme(
    text = element_text(family = "Times"),           # Set default font for all text
    plot.title = element_text(family = "Times"),     # Title font
    plot.subtitle = element_text(family = "Times"),  # Subtitle font
    axis.title = element_text(family = "Times"),     # Axis title font
    axis.text = element_text(family = "Times"),      # Axis text font
    legend.text = element_text(family = "Times"),    # Legend text font
    legend.title = element_text(family = "Times"), 
    panel.grid = element_blank(), 
    panel.border = element_rect(colour = "black", fill = NA),
    axis.ticks.length = unit(.15, "cm"), 
    strip.background = element_blank()
  ) 

theme_set(custom_theme)

###################################################
# calculation of prior distribution
###################################################

df_prior <- df |> 
  mutate(p1i_prior = p1i_prior / 100,
         p2i_prior = p2i_prior / 100) |> 
  #filter(prior_correct == 0 & post_correct == 0)
  filter(prior_correct == 0) 

### global 
mu_overall <- mean(df_prior$y1i_prior, na.rm = TRUE)
sigma_overall <- (mean(df_prior$y3i_prior) - mean(df_prior$y2i_prior)) / (qnorm(1 - mean(df_prior$p2i_prior)) - qnorm(mean(df_prior$p1i_prior))) 


df_prior <- df |>  
  filter(prior_correct == 0) |> 
  mutate(p1i_prior = p1i_prior / 100,
         p2i_prior = p2i_prior / 100) |>
  # round to 2 decimal
  mutate(p1i_prior = round(p1i_prior, 2),
         p2i_prior = round(p2i_prior, 2)) |>
  # if under 0.01, set to 0.01 
  mutate(p1i_prior = ifelse(p1i_prior < 0.01, 0.01, p1i_prior),
         p2i_prior = ifelse(p2i_prior < 0.01, 0.01, p2i_prior)) 

params_df <- df_prior  |> 
  rowwise() |> 
  mutate(
    mu_est = y1i_prior,
    sd_est = max(
      (y3i_prior - y2i_prior) / (qnorm(1 - p2i_prior) - qnorm(p1i_prior)),
      1 # Impose SD of minimum 1 for plot readability 
    )
  ) |> 
  ungroup()  |> 
  select(mean = mu_est, sd = sd_est) 

# bind params to df_prior 
df_prior <- df_prior |> 
  bind_cols(params_df) |> 
  select(-y1i_prior, -y2i_prior, -y3i_prior, -p1i_prior, -p2i_prior) |> 
  filter(sd <= 40) |>  # filter nonsensically large sd's
  # round to whole digits 
  mutate(sd = round(sd, 0)) |>
  #mutate(sd = round(sd * 2) / 2) |>  # round sds to whole or half digits 
  # if above 10, set to 10 
  mutate(sd = ifelse(sd > 10, 10, sd))

# check prior sd bins 
df_prior |> 
  group_by(sd) |> 
  summarise(n = n()) |> 
  mutate(prop = n / sum(n)) |> 
  arrange(desc(sd)) 


#save for model fitting
save(df_prior, file = "data/df_prior.RData")

params_df <- params_df |>
  filter(sd <= 40) # filter nonsensically large sd's 

####################################
# plotting
####################################

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
gg.prior <- ggplot(plot_data, aes(x = x, y = density, group = respondent)) +
  geom_line(alpha = .05, color = "#B06767", lwd = .5) +
  geom_line(aes(y = o_density), color = "#911C1C", linewidth = .3, alpha = .5) + 
  labs(x = "Expected price change", 
       y = "Density") +
  theme(legend.position = "none") + 
  guides(x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE)) + 
  facet_wrap(~"Fitted prior") + 
  xlim(-50, 50) +
  annotate("text", x = 35, y = .35, 
           label = mu_sd_temp, size = 2.5)

# compute probability of being above 0 
pnorm(0, mean = mu_overall, sd = sigma_overall, lower.tail = FALSE)

# density between -1 sd and 1 sd 
pnorm((mu_overall+sigma_overall), mean = mu_overall, sd = sigma_overall) - pnorm((mu_overall-sigma_overall), mean = mu_overall, sd = sigma_overall)

## show number of NAs
sum(is.na(params_df$sd))

## filter out NAs
params_df <- params_df |> 
  filter(!is.na(sd))

## negative sigma? 
params_df |> 
  filter(sd < 0) |> 
  nrow()


### 2d density plot 
params_df |>  
  filter(sd <= 40) |> 
  ggplot(aes(x = mean, y = sd)) + 
  geom_point(shape = 1, color = "#B06767") + 
  labs(x = "Mean", y = "SD") + 
  theme(legend.position = "none") + 
  guides(x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE)) + 
  geom_density_2d(color = "#911C1C") +
  xlim(-50, 50) 
  #geom_smooth(method = "lm", color = "#911C1C", se = FALSE)
#geom_vline(xintercept = mu_overall, linetype = "dashed", color = "green") + 
#geom_hline(yintercept = sigma_overall, linetype = "dashed", color = "green") + 
#annotate("text", x = 60, y = 10, label = paste("Mean =", round(mu_overall, 2), "\n", "SD =", round(sigma_overall, 2)), size = 3, color = "black", hjust = 0)


