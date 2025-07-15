
# Function to estimate normal parameters via likelihood maximization
estimate_mle <- function(y1i_prior, y2i_prior, y3i_prior, p1i_prior, p2i_prior) {
  # Define the negative log-likelihood function (we minimize this)
  neg_log_likelihood <- function(params) {
    mu <- params[1]
    sigma <- params[2]
    
    if (sigma <= 0) return(Inf)  # Ensure sigma remains positive
    
    # Expected values based on the normal distribution
    exp_mean  <- mu
    exp1i <- mu + sigma * qnorm(p1i_prior)
    exp2i <- mu + sigma * qnorm(1 - p2i_prior)
    
    # Log-likelihood function (equivalent to squared error minimization)
    log_likelihood <- -((y1i_prior - exp_mean)^2 + (y2i_prior - exp1i)^2 + (y3i_prior - exp2i)^2)
    
    return(-log_likelihood)  # Negate to maximize likelihood
  }
  
  # Initial guesses
  sigma_lower <- (y1i_prior - y2i_prior) / abs(qnorm(p1i_prior))
  sigma_upper <- (y3i_prior - y1i_prior) / qnorm(1 - p2i_prior)
  init_sigma <- (sigma_lower + sigma_upper) / 2
  init_params <- c(mu = y1i_prior, sigma = init_sigma)
  
  # Optimize using "L-BFGS-B" method, ensuring sigma > 0
  result <- optim(init_params, neg_log_likelihood, method = "L-BFGS-B", lower = c(-Inf, 1e-6))
  
  return(result$par)  # Returns estimated (mu, sigma)
}

# Compute MLE estimates
estimates <- apply(df_prior, 1, function(row) {
  estimate_mle(
    y1i_prior  = as.numeric(row["y1i_prior"]),
    y2i_prior = as.numeric(row["y2i_prior"]),
    y3i_prior = as.numeric(row["y3i_prior"]),
    p1i_prior = as.numeric(row["p1i_prior"]),
    p2i_prior = as.numeric(row["p2i_prior"])
  )
})

# Convert to data frame
estimates_df <- data.frame(t(estimates))
colnames(estimates_df) <- c("mu_est", "sigma_est")

# Combine with original data
result_df <- cbind(df_prior, estimates_df)
print(result_df)


# Compute overall mean and standard deviation
mu_overall_MLE <- mean(estimates_df$mu_est, na.rm = TRUE)
sigma_overall_MLE <- sqrt(mean(estimates_df$sigma_est^2, na.rm = TRUE))

# Define a range of x-values for the plot (e.g., from 0 to 100)

x_values <- seq(-50, 50, length.out = 500)

# Prepare the plot data
plot_data <- data.frame(
  x = rep(x_values, nrow(result_df)),  # Repeat x_values for each respondent
  mean = rep(result_df$mu_est, each = length(x_values)),  # Use 'mu_est' as mean
  sd = rep(result_df$sigma_est, each = length(x_values)),  # Use 'sigma_est' as standard deviation
  respondent = rep(rownames(result_df), each = length(x_values)),
  o_mean = rep(mu_overall_MLE),
  o_sigma = rep(sigma_overall_MLE) # Use row names as respondent IDs
)

# Calculate the density values for each respondent's normal distribution
plot_data$density <- dnorm(plot_data$x, mean = plot_data$mean, sd = plot_data$sd) 
plot_data$o_density <- dnorm(plot_data$x, mean = plot_data$o_mean, sd = plot_data$o_sigma)

ggplot(plot_data, aes(x = x, y = density, group = respondent)) +
  geom_line(alpha = .1, color = "#B06767") +
  geom_line(aes(y = o_density), color = "#911C1C", linewidth = .3, alpha = .5) + 
  labs(x = "Effect size guess", y = "Density") +
  theme(legend.position = "none") + 
  guides(x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE)) + 
  facet_wrap(~"Fitted Normal Distributions MLE") + 
  xlim(-50, 50)