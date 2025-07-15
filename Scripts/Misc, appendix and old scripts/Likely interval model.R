###################################
# Setup 
###################################

library(brms) 
library(tidyverse)
library(survey)
library(ggdist)

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

# complete cases 
df_clean <- df_prior |> 
  filter(!is.na(sex), 
         !is.na(agegroup), 
         !is.na(educ), 
         !is.na(region), 
         !is.na(educ)) 

#df_clean <- df_prior |> 
  #filter(!is.na(region), 
         #!is.na(educ)) 

# population margins 
N <- 4266387 ## population  

sex_pop <- data.frame(sex = c("1", "2"), 
                      Freq = c(0.493*N, 0.507*N))

age_pop <- data.frame(agegroup = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"), 
                        Freq = c(0.186*N, 0.137*N, 0.147*N, 0.175*N, 0.152*N, 0.202*N)) 

educ_pop <- data.frame(educ = c(1, 2, 3, 4, 5, 6), 
                       Freq = c(0.229*N, 0.10*N, 0.26*N, 0.049*N, 0.172*N, 0.115*N))

region_pop <- data.frame(region = c(1, 2, 3, 4, 5), 
                       Freq = c(0.303*N, 0.149*N, 0.212*N, 0.231*N, 0.105*N)) 

design_unweighted <- svydesign(ids = ~1, data = df_clean) 

# rake
design_weighted <- rake(
  design = design_unweighted,
  sample.margins = list(~sex, ~agegroup, ~educ, ~region),
  population.margins = list(sex_pop, age_pop, educ_pop, region_pop), 
  control = list(maxit = 50, epsilon = 1e-6)
) 

# rake
#design_weighted <- rake(
  #design = design_unweighted,
  #sample.margins = list(~educ, ~region),
  #population.margins = list(educ_pop, region_pop)
#) 

df_clean$weights <- weights(design_weighted) 
df_clean$weights <- df_clean$weights / mean(df_clean$weights) # rescale weights to mean of 1

hist(df_clean$weights) 

mean(df_clean$weights)

# trim weights 
df_clean$weights[df_clean$weights > 5] <- 5 # trim weights U
df_clean$weights[df_clean$weights < 0.2] <- 0.2 # trim weights L


# trim weights


# fit raw models 
m_y1_raw <- brm(data = df_prior, 
                family = gaussian, 
                y1i_prior ~ 1,
                prior = c(prior(normal(0, 12.30534), class = Intercept),# 0, sd(df$y1i_prior)
                          prior(exponential(12.30534), class = sigma)), 
                
                warmup = 1000,
                iter = 2000, 
                chains = 4,
                cores = 4,
                seed = 6582) 

summary(m_y1_raw) ## diagnostics 

m_y2_raw <- brm(data = df_prior, 
                family = gaussian, 
                y2i_prior ~ 1,
                prior = c(prior(normal(0, 12.49888), class = Intercept),# 0, sd(df$y2i_prior)
                          prior(exponential(12.49888), class = sigma)), 
                
                warmup = 1000,
                iter = 2000, 
                chains = 4,
                cores = 4,
                seed = 6582) 

summary(m_y2_raw) ## diagnostics

m_y3_raw <- brm(data = df_prior, 
                family = gaussian, 
                y3i_prior ~ 1,
                prior = c(prior(normal(0, 15.34496), class = Intercept),# 0, sd(df$y3i_prior)
                          prior(exponential(15.34496), class = sigma)), 
                
                warmup = 1000,
                iter = 2000, 
                chains = 4,
                cores = 4,
                seed = 6582)

summary(m_y3_raw) ## diagnostics

## fit weighted models 
m_y1_weighted <- brm(data = df_clean, 
                      family = gaussian, 
                      y1i_prior | weights(weights) ~ 1,
                      prior = c(prior(normal(0, 12.30534), class = Intercept),# 0, sd(df$y1i_prior)
                                prior(exponential(12.30534), class = sigma)), 
                      warmup = 1000,
                      iter = 2000, 
                      chains = 4,
                      cores = 4,
                      seed = 6582)


summary(m_y1_weighted) ## diagnostics

m_y2_weighted <- brm(data = df_clean, 
                      family = gaussian, 
                      y2i_prior | weights(weights) ~ 1,
                      prior = c(prior(normal(0, 12.49888), class = Intercept),# 0, sd(df$y2i_prior)
                                prior(exponential(12.49888), class = sigma)), 
                      warmup = 1000,
                      iter = 2000, 
                      chains = 4,
                      cores = 4,
                      seed = 6582)

summary(m_y2_weighted) ## diagnostics

m_y3_weighted <- brm(data = df_clean, 
                      family = gaussian, 
                      y3i_prior | weights(weights) ~ 1,
                      prior = c(prior(normal(0, 15.34496), class = Intercept),# 0, sd(df$y3i_prior)
                                prior(exponential(15.34496), class = sigma)), 
                      warmup = 1000,
                      iter = 2000, 
                      chains = 4,
                      cores = 4,
                      seed = 6582)

summary(m_y3_weighted) ## diagnostics 

###############################################
## plot 
###############################################

# convenience function to extrat MCMC draws 
process_model <- function(model, parameter, weighted) {
  model_df <- as_draws_df(model) |> 
    select(Intercept) |> 
    mutate(model = weighted, 
           value = parameter) |> 
    rename(alpha = Intercept) #|> 
    # keep only 95% hdi 
    #filter( 
      #alpha >= quantile(alpha, 0.025),
      #alpha <= quantile(alpha, 0.975)) 
}

y1_raw_df <- process_model(m_y1_raw, "y1i", "Unweighted")
y1_weighted_df <- process_model(m_y1_weighted, "y1i", "Weighted")
y2_raw_df <- process_model(m_y2_raw, "y2i", "Unweighted")
y2_weighted_df <- process_model(m_y2_weighted, "y2i", "Weighted")
y3_raw_df <- process_model(m_y3_raw, "y3i", "Unweighted")
y3_weighted_df <- process_model(m_y3_weighted, "y3i", "Weighted")

# combine into one dataframe
y_df <- bind_rows(y1_raw_df, y1_weighted_df, 
                  y2_raw_df, y2_weighted_df, 
                  y3_raw_df, y3_weighted_df)
  
## geom_slab 
y_df |> 
  ggplot(aes(x = alpha, y = value, fill = model)) +
  stat_slab(aes(y = value), color = NA, lwd = .5, alpha = .9) + 
  xlim(-15, 15) + 
  scale_fill_manual(values = c("#B06767", "#911C1C")) +
  labs(x = "", y = "", fill = "") +
  guides(x = guide_axis(minor.ticks = TRUE), 
         fill  = guide_legend(position = "inside")) + 
  theme(panel.grid.major.y = element_line(color = "grey50", linetype = "dotted", size = 0.5), 
        legend.position.inside = c(0.15, 0.9), 
        legend.key.size = unit(0.1, 'cm')) 


lm(y1i_prior ~ 1, data = df_clean) |> 
  summary()

lm(y1i_prior ~ 1, weights = weights, data = df_clean) |>
  summary()

summary(m_y1_weighted)
 

            