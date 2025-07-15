library(ggplot2)
library(dplyr)
library(brms)


eq8 <- brm(
  update ~ 0 + e_prior,
  data = df,
  family = student(),
  prior = c(
    prior(normal(0, 12.07), class = "b"),
    prior(exponential(12.07), class = "sigma")
  ),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4, 
  seed = 6582)

df <- df %>%
  mutate(e_prior_bin = cut(e_prior, breaks = 20))

# Get fitted values using a sequence over e_prior
new_data <- data.frame(e_prior = seq(min(df$e_prior), max(df$e_prior), length.out = 100))
fitted_vals <- fitted(eq8, newdata = new_data, re_formula = NA, summary = TRUE) %>%
  as.data.frame() %>%
  bind_cols(new_data)

# Plot
ggplot() +
  geom_point(data = df, aes(x = e_prior, y = update), 
             alpha = 0.4, color = "blue") +
  geom_line(data = fitted_vals, aes(x = e_prior, y = Estimate), color = "red", size = 1) +
  geom_ribbon(data = fitted_vals, aes(x = e_prior, ymin = Q2.5, ymax = Q97.5), 
              fill = "red", alpha = 0.2) +
  geom_histogram(data = df, aes(x = e_prior, y = ..density..), 
                 bins = 20, fill = "grey80", alpha = 0.4) +
  labs(title = "Model Slope with Raw Data and 20 Bins Histogram",
       y = "Update / Density",
       x = "e_prior") +
  theme_minimal()

# Generate new data for predictions
new_data <- data.frame(e_prior = seq(-60, 60, length.out = 100))

# Get model predictions (posterior summaries)
fitted_df <- fitted(eq8, newdata = new_data, re_formula = NA, summary = TRUE) %>%
  as.data.frame() %>%
  bind_cols(new_data)

# Plot
ggplot(df, aes(x = e_prior, y = update)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  
  # Raw data points
  geom_point(alpha = 0.3, size = 1, color = "#E6CBCB") +
  geom_smooth(aes(x = e_prior, y = update), 
              color = "#B06767", se = FALSE, size = 1, alpha = 0.8) +
  
  # Model prediction line
  geom_line(data = fitted_df, 
            aes(x = e_prior, y = Estimate), 
            color = "#911C1C", linewidth = 1, alpha = 0.8) +
  
  # Binned means (20 bins)
  stat_summary_bin(fun = mean, bins = 20, geom = "point", size = 1, color = "#911C1C") +
  
  # Credible interval ribbon
  geom_ribbon(data = fitted_df, 
              aes(x = e_prior, ymin = Q2.5, ymax = Q97.5), 
              fill = "#911C1C", alpha = 0.2, inherit.aes = FALSE) +
  
  labs(
    x = "Signal - Prior",
    y = expression(paste(Delta, " Belief"))
  ) +
  ylim(-60, 60) +
  xlim(-60, 60) 


# Get model predictions at observed values
fitted_vals <- fitted(eq8, re_formula = NA, summary = TRUE) %>%
  as.data.frame() %>%
  bind_cols(df)

# Plot
ggplot(fitted_vals, aes(x = e_prior, y = Estimate)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  # Raw data points
  geom_point(data = df, aes(x = e_prior, y = update), 
             alpha = 0.4, color = "#E6CBCB", size = 1) +
  geom_smooth(data = df, aes(x = e_prior, y = update), 
              color = "#E6CBCB",se = F, size = 1) +
  
  # Predicted posterior mean points
  geom_point(color = "#B06767", size = 1) +
  
  # 95% credible interval error bars
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), 
                width = 0.1, color = "#B06767", alpha = 0.6) +
  
  # Binned means of raw data
  stat_summary_bin(data = df, 
                   aes(x = e_prior, y = update), 
                   fun = mean, bins = 20, geom = "point", 
                   size = 1, color = "#911C1C") +
  
  labs(
    title = "Model Predictions with Raw Data and Binned Means",
    y = "Update",
    x = "Prior (e_prior)"
  ) + 
  theme_minimal() 

pred_vals <- predict(eq8, re_formula = NA, summary = TRUE) %>%
  as.data.frame() %>%
  bind_cols(df)

# Get fitted values (mean prediction, excludes sigma)
fitted_vals <- fitted(eq8, re_formula = NA, summary = TRUE) %>%
  as.data.frame() %>%
  bind_cols(df)

# Plot
ggplot(fitted_vals, aes(x = e_prior, y = Estimate)) +
  
  # 95% prediction interval (includes sigma)
  geom_errorbar(data = pred_vals, 
                aes(x = e_prior, ymin = Q2.5, ymax = Q97.5), 
                width = 0.3, color = "#E6CBCB", alpha = 0.3, inherit.aes = FALSE) + 
  
  # Model predicted mean
  geom_point(color = "#B06767", size = 1) +
  
  # 95% credible interval of mean prediction
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), 
                width = 0.1, color = "#B06767", alpha = 0.6) +
  
  # Binned means of raw data
  stat_summary_bin(data = df, 
                   aes(x = e_prior, y = update), 
                   fun = mean, bins = 20, geom = "point", 
                   size = 1.5, color = "black") +
  
  labs(
    title = "Model Predictions with Raw Data and Total Uncertainty (Includes Sigma)",
    y = "Update",
    x = "Prior (e_prior)"
  ) 

