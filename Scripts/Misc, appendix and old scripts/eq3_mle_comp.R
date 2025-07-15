library(tidyverse)

#################
# Simulate data
#################

set.seed(42)  # Set seed for reproducibility

# Number of observations
n <- 1000

# Simulate the most likely vote share (y1i) as random values between 50 and 70
y1i <- runif(n, -5, 10)

# Simulate the lower and upper bounds (y2i and y3i) as random values within reasonable ranges of y1i
y2i <- y1i - runif(n, 5, 10)  # Lower bound is a bit less than y1i
y3i <- y1i + runif(n, 5, 10)  # Upper bound is a bit more than y1i

# Simulate probabilities for the lower and upper tails (p1i and p2i) between 0.05 and 0.2
p1i <- runif(n, 0.05, 0.2)
p2i <- runif(n, 0.05, 0.2) 

# Create a data frame
responses <- data.frame(
  Respondent = paste0("", 1:n),  # Respondent identifiers (R1, R2, ..., R1000)
  y1i = y1i,
  y2i = y2i,
  y3i = y3i,
  p1i = p1i,
  p2i = p2i
)

#############
# EQ3 
############# 

### global 
mu_overall_eq3 <- mean(responses$y1i, na.rm = TRUE)
sigma_overall_eq3 <- (mean(responses$y3i) - mean(responses$y2i)) / (qnorm(1 - mean(responses$p2i)) - qnorm(mean(responses$p1i))) 


params_df <- responses %>%
  rowwise() %>%
  mutate(
    mu_est = y1i,
    sd_est = max(
      (y3i - y2i) / (qnorm(1 - p2i) - qnorm(p1i)),
      1
    )
  ) %>%
  ungroup() %>%
  select(mean = mu_est, sd = sd_est)


# Create a sequence of x values for plotting
x_values <- seq(-50, 50, length.out = 500)

# Create a data frame for plotting
plot_data <- data.frame(x = rep(x_values, nrow(params_df)),
                        mean = rep(params_df$mean, each = length(x_values)),
                        sd = rep(params_df$sd, each = length(x_values)),
                        respondent = rep(rownames(params_df), each = length(x_values)), 
                        o_mean = rep(mu_overall_eq3),
                        o_sigma = rep(sigma_overall_eq3)
)

# Calculate the density values for each respondent
plot_data$density <- dnorm(plot_data$x, mean = plot_data$mean, sd = plot_data$sd)
plot_data$o_density <- dnorm(plot_data$x, mean = plot_data$o_mean, sd = plot_data$o_sigma)

# Plot
ggplot(plot_data, aes(x = x, y = density, group = respondent)) +
  geom_line(alpha = .1, color = "#B06767") +
  geom_line(aes(y = o_density), color = "#911C1C", linewidth = .3, alpha = .5) + 
  labs(x = "Effect size guess", y = "Density") +
  theme(legend.position = "none") + 
  guides(x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE)) + 
  facet_wrap(~"Fitted Normal Distributions EQ3") + 
  xlim(-50, 50)


#######################
# MLE 
#######################

# Function to estimate normal parameters via likelihood maximization
estimate_mle <- function(y1i, y2i, y3i, p1i, p2i) {
  # Define the negative log-likelihood function (we minimize this)
  neg_log_likelihood <- function(params) {
    mu <- params[1]
    sigma <- params[2]
    
    if (sigma <= 0) return(Inf)  # Ensure sigma remains positive
    
    # Expected values based on the normal distribution
    exp_mean  <- mu
    exp1i <- mu + sigma * qnorm(p1i)
    exp2i <- mu + sigma * qnorm(1 - p2i)
    
    # Log-likelihood function (equivalent to squared error minimization)
    log_likelihood <- -((y1i - exp_mean)^2 + (y2i - exp1i)^2 + (y3i - exp2i)^2)
    
    return(-log_likelihood)  # Negate to maximize likelihood
  }
  
  # Initial guesses
  sigma_lower <- (y1i - y2i) / abs(qnorm(p1i))
  sigma_upper <- (y3i - y1i) / qnorm(1 - p2i)
  init_sigma <- (sigma_lower + sigma_upper) / 2
  init_params <- c(mu = y1i, sigma = init_sigma)
  
  # Optimize using "L-BFGS-B" method, ensuring sigma > 0
  result <- optim(init_params, neg_log_likelihood, method = "L-BFGS-B", lower = c(-Inf, 1e-6))
  
  return(result$par)  # Returns estimated (mu, sigma)
}

# Compute MLE estimates
estimates <- apply(responses, 1, function(row) {
  estimate_mle(
    y1i  = as.numeric(row["y1i"]),
    y2i = as.numeric(row["y2i"]),
    y3i = as.numeric(row["y3i"]),
    p1i = as.numeric(row["p1i"]),
    p2i = as.numeric(row["p2i"])
  )
})

# Convert to data frame
estimates_df <- data.frame(t(estimates))
colnames(estimates_df) <- c("mu_est", "sigma_est")

# Combine with original data
result_df <- cbind(responses, estimates_df)
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



#################################
## Compare parameters overall 
#################################


overall_params <- data.frame(
  Method = c("EQ3", "MLE"),
  Mean = c(mu_overall_eq3, mu_overall_MLE),
  SD = c(sigma_overall_eq3, sigma_overall_MLE)
) |> 
  print()



