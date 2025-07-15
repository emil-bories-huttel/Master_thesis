# import dfs and packages from "4 bayesian model fitting"
# before running this code chunk 

brm(
  y_dev ~ time,
  data = df_long_p,
  family = gaussian(),
  prior = c(
    prior(normal(0, 11.9), class = "Intercept"), 
    prior(normal(0, 11.9), class = "b"),
    prior(exponential(11.9), class = "sigma")
  ),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4, 
  seed = 6582) 


brm(
  y_dev ~ time + (1|id) + age + lr_scale + sex + 
    as.factor(educ) + as.factor(region) + as.factor(urban),
  data = df_long_p,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1.54), class = "Intercept"),
    prior(normal(0, 1.54), class = "b"),
    prior(exponential(1.54), class = "sigma"),
    prior(exponential(1.54), class = "sd")
  ),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4, 
  seed = 6582) 

brm(
  y_dev ~ time + (1|id),
  data = df_long_p,
  family = gaussian(),
  prior = c(
    prior(normal(0, 11.9), class = "Intercept"),
    prior(normal(0, 11.9), class = "b"),
    prior(exponential(11.9), class = "sigma"),
    prior(exponential(11.9), class = "sd")
  ),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4, 
  seed = 6582) 



1.25^2/(1.25^2 + 0.48^2)

brm(
  y_dev ~ time + as.factor(id),
  data = df_long_p,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1.54), class = "Intercept"),
    prior(normal(0, 1.54), class = "b"),
    prior(exponential(1.54), class = "sigma")
  ),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4, 
  seed = 6582) 



df_long_prox <- df |> 
  mutate(id = row_number()) |> 
  select(proximity_prior, proximity_post, id, treat, age, lr_scale, sex, educ, region, urban, dist, tenure) |> 
  pivot_longer(cols = c(proximity_prior, proximity_post), names_to = "time", values_to = "y_dev") |> 
  mutate(time = case_when(
    time == "proximity_prior" ~ 0,
    time == "proximity_post" ~ 1
  )) 

brm(
  y_dev ~ time + (1|id + dist) + age + lr_scale + sex + 
    as.factor(educ) + as.factor(region) + as.factor(urban),
  data = df_long_prox,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1.54), class = "Intercept"),
    prior(normal(0, 1.54), class = "b"),
    prior(exponential(1.54), class = "sigma"),
    prior(exponential(1.54), class = "sd")
  ),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4, 
  seed = 6582) 

sd(df_long_prox$y_dev, na.rm = T)

lm(y_dev ~ time*as.factor(dist), data = df_long_prox) |> 
  summary()

