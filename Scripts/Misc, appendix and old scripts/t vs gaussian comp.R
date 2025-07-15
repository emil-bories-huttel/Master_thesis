library(brms)
library(loo)


# with intercept 
eq8_1_a <- brm(
  update ~ e_prior,
  data = df,
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

summary(eq8_1) ## quick diagnostics

# without intercept
eq8_1_a <- brm(
  update ~ 0 + e_prior,
  data = df,
  family = gaussian(),
  prior = c(
    prior(normal(0, 12.07), class = "b"),
    prior(exponential(12.07), class = "sigma")
  ),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4, 
  seed = 6582) 

summary(eq8_1) ## quick diagnostics

## compare models (is no intercept better fit?) 
# essentially the same 

loo1 <- loo(eq8_1)
loo1a <- loo(eq8_1_a)
loo_compare(loo1, loo1a)

# bayesian r_squared 
bayes_R2(eq8_1) 
bayes_R2(eq8_1_a)


# gaussian interaction
eq8_2 <- brm(
  update ~ 0 + e_prior*cred_d,
  data = df,
  family = gaussian(),
  prior = c(
    prior(normal(0, 12.07), class = "b"),
    prior(exponential(12.07), class = "sigma")
  ),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4, 
  seed = 6582)

# student t interaction 
eq8_2t <- brm(
  update ~ 0 + e_prior*cred_d,
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

summary(eq8_2) ## quick diagnostics

summary(eq8_2t) ## quick diagnostics

# see priors 
prior_summary(eq8_2t)

loo2 <- loo(eq8_2)
loo2t <- loo(eq8_2t)

loo_compare(loo2, loo2t)

bayes_R2(eq8_2)
bayes_R2(eq8_2t)

pp_check(eq8_2t) + xlim(-50, 50) 

pp_check(eq8_2)


eq8_3t <- brm(
  update ~ 0 + e_prior*treat,
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

eq8_3 <- brm(
  update ~ 0 + e_prior*treat,
  data = df,
  family = gaussian(),
  prior = c(
    prior(normal(0, 12.07), class = "b"),
    prior(exponential(12.07), class = "sigma")
  ),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4, 
  seed = 6582)

loo3 <- loo(eq8_3)
loo3t <- loo(eq8_3t)

loo_compare(loo3, loo3t)

pp_check(eq8_3t) + xlim(-50, 50) 
pp_check(eq8_3) + xlim(-50, 50) 


## mlm compare 

m2 <- brm(data = df, 
           family = gaussian(), 
           update ~ 0 + e_prior + (0 + e_prior | credibility),
           prior = c(prior(beta(2, 2), lb = 0, ub = 1, class = b),
                     prior(exponential(11.24), class = sigma),
                     prior(exponential(.2), class = sd)
           ),
           
           warmup = 1000,
           iter = 2000, 
           chains = 4,
           cores = 4,
           seed = 6582) 


m2t <- brm(data = df, 
           family = student(), 
           update ~ 0 + e_prior + (0 + e_prior | credibility),
           prior = c(prior(beta(2, 2), lb = 0, ub = 1, class = b),
                     prior(exponential(11.24), class = sigma),
                     prior(exponential(.2), class = sd)
           ),
           
           warmup = 1000,
           iter = 2000, 
           chains = 4,
           cores = 4,
           seed = 6582) 


loo_m2 <- loo(m2)
loo_m2t <- loo(m2t)

loo_compare(m2, m2t)


m2_2 <- brm(data = df_prior, 
           family = student(), 
           update ~ 0 + e_prior + (0 + e_prior | sd),
           prior = c(prior(beta(2, 2), lb = 0, ub = 1, class = b),
                     prior(exponential(11.24), class = sigma),
                     prior(exponential(.2), class = sd)
           ),
           
           warmup = 1000,
           iter = 2000, 
           chains = 4,
           cores = 4,
           seed = 6582) 

