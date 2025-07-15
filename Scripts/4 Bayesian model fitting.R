
load("data/cleaned_data.RData") 
load("data/df_prior.RData")

# load packages 
library(brms) 
library(tidyverse)
library(survey)
library(ggdist)
library(bayesplot)

df_long_b <- df |> 
  mutate(id = row_number()) |> 
  select(treat, y1i_prior, y1i_post, age, lr_scale, sex, educ, region, urban, id) |> 
  pivot_longer(cols = c(y1i_prior, y1i_post), names_to = "time", values_to = "y") |> 
  mutate(time = case_when(
    time == "y1i_prior" ~ 0,
    time == "y1i_post" ~ 1
  )) 

df_long_p <- df |> 
  mutate(id = row_number()) |> 
  select(pro_dev_prior, pro_dev_post, id, treat, age, lr_scale, sex, educ, region, urban, dist, tenure) |> 
  pivot_longer(cols = c(pro_dev_prior, pro_dev_post), names_to = "time", values_to = "y_dev") |> 
  mutate(time = case_when(
    time == "pro_dev_prior" ~ 0,
    time == "pro_dev_post" ~ 1
  )) 

# turn of scientific notation
options(scipen = 999)

################################
# Weights
################################

df_prior <- df |> 
  mutate(p1i_prior = p1i_prior / 100,
         p2i_prior = p2i_prior / 100) |> 
  filter(prior_correct == 0)

# complete cases 
df_clean <- df_prior |> 
  filter(!is.na(sex), 
         !is.na(agegroup), 
         !is.na(educ), 
         !is.na(region))

#df_clean <- df_prior |> 
#filter(!is.na(region), 
#!is.na(educ)) 

# population margins 
N <- 4266387 ## population  

sex_pop <- data.frame(sex = c("0", "1"), 
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
  sample.margins = list(~sex, ~agegroup, ~educ),
  population.margins = list(sex_pop, age_pop, educ_pop), 
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


##############################################
# Likely intervals eq4 
##############################################

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

summary(m_y1_raw) ## quick diagnostics 

#save 
saveRDS(m_y1_raw, "Models/m_y1_raw.rds")

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

summary(m_y2_raw) ## quick diagnostics

#save
saveRDS(m_y2_raw, "Models/m_y2_raw.rds")

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

summary(m_y3_raw) ## quick diagnostics

saveRDS(m_y3_raw, "Models/m_y3_raw.rds")

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


summary(m_y1_weighted) ## quick diagnostics

#save
saveRDS(m_y1_weighted, "Models/m_y1_weighted.rds")

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

summary(m_y2_weighted) ## quick diagnostics

saveRDS(m_y2_weighted, "Models/m_y2_weighted.rds")

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

summary(m_y3_weighted) ## quick diagnostics 

saveRDS(m_y3_weighted, "Models/m_y3_weighted.rds")


##############################################
# pre-post eq 5
##############################################

## simple model

eq5_1 <- brm(
  y ~ time,
  data = df_long_b,
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

summary(eq5_1) ## quick diagnostics

mcmc_trace(eq5_1)

saveRDS(eq5_1, "Models/eq5_1.rds")

# predict y at time = 1 
newdata_eq5_1 <- data.frame(time = 1)

fitted(eq5_1, newdata = newdata_eq5_1, probs = c(0.025, 0.975))

# posterior predictive check
pp_check(eq5_1) 

# fixed effects model

eq5_2 <- brm(
  y ~ time + as.factor(id),
  data = df_long_b,
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

summary(eq5_2) ## quick diagnostics

#mcmc_trac(eq5_2)

pp_check(eq5_2) 

saveRDS(eq5_2, "Models/eq5_2.rds")

# random effects model 

eq5_3 <- brm(
  y ~ time + (1|id),
  data = df_long_b,
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

summary(eq5_3)

#mcmc_trace(eq5_3)

saveRDS(eq5_3, "Models/eq5_3.rds")

pp_check(eq5_3) 

# random effects model 

eq5_4 <- brm(
  y ~ time + (1|id) + age + lr_scale + sex + 
    as.factor(educ) + as.factor(region) + as.factor(urban),
  data = df_long_b,
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

summary(eq5_4)

#mcmc_trace(eq5_3)

saveRDS(eq5_4, "Models/eq5_4.rds")

pp_check(eq5_4) 

#intraclass correlation 
0.09^2/(0.09^2 + 10.42^2)

#################################
# Treatment EQ 6 
#################################

## simple model
eq6_1 <- brm(
  y1i_post ~ treat,
  data = df,
  family = gaussian(),
  prior = c(
    prior(normal(0, 11.24), class = "Intercept"),
    prior(normal(0, 11.24), class = "b"),
    prior(exponential(11.24), class = "sigma")
  ),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4, 
  seed = 6582) 

summary(eq6_1) ## quick diagnostics

mcmc_trace(eq6_1)

pp_check(eq6_1)

saveRDS(eq6_1, "Models/eq6_1.rds") 

# predict y at treat = 1 
newdata_eq6_1 <- data.frame(treat = 1)

fitted(eq6_1, newdata = newdata_eq6_1, probs = c(0.025, 0.975))


eq6_2 <- brm(
  y1i_post ~ treat + y1i_prior,
  data = df,
  family = gaussian(),
  prior = c(
    prior(normal(0, 11.24), class = "Intercept"),
    prior(normal(0, 11.24), class = "b"),
    prior(exponential(11.24), class = "sigma")
  ),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4, 
  seed = 6582) 

summary(eq6_2) ## quick diagnostics

mcmc_trace(eq6_2)

pp_check(eq6_2)

saveRDS(eq6_2, "Models/eq6_2.rds") 

# covariates model 
eq6_3 <- brm(
  y1i_post ~ treat + y1i_prior + age + lr_scale + sex + 
    as.factor(educ) + as.factor(region) + as.factor(urban),
  data = df,
  family = gaussian(),
  prior = c(
    prior(normal(0, 11.24), class = "Intercept"),
    prior(normal(0, 11.24), class = "b"),
    prior(exponential(11.24), class = "sigma")
  ),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4, 
  seed = 6582) 

summary(eq6_3) ## quick diagnostics

mcmc_trace(eq6_3)

pp_check(eq6_3)

saveRDS(eq6_3, "Models/eq6_3.rds")

# first difference model 
eq6_4 <- brm(
  update ~ treat,
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

summary(eq6_4) ## quick diagnostics

mcmc_trace(eq6_4)

pp_check(eq6_4)

saveRDS(eq6_4, "Models/eq6_4.rds") 

# predict y at treat = 1 
newdata_eq6_4 <- data.frame(treat = 1)

fitted(eq6_4, newdata = newdata_eq6_4, probs = c(0.025, 0.975))

intercept4_test <- eq6_4 |> 
  spread_draws(b_Intercept, b_treat) |> 
  mutate(
    fitted_treat1 = b_Intercept + b_treat,
    ratio = fitted_treat1 / b_Intercept
  )

# Compute probability that fitted at treat = 1 is between 3.5 * intercept or 4.5 * intercept 
intercept4_test |> 
  summarise(prob_between = mean(ratio > 3.5 & ratio < 4.5))

##################################
# Bayesian behaviour EQ7 
##################################

eq7 <- brm(data = df, 
          family = gaussian, 
          y1i_post ~ 0 + y1i_prior + signal,
          prior = c(prior(beta(2, 2), lb = 0, ub = 1, class = b),
                    prior(exponential(11.24), class = sigma)), #exp(sd(df$y1i_post)
          
          warmup = 1000,
          iter = 2000, 
          chains = 4,
          cores = 4,
          seed = 6582) 

hypothesis(eq7, "y1i_prior + signal = 1")

summary(eq7) ## quick diagnostics

mcmc_trace(eq7)

pp_check(eq7)

saveRDS(eq7, "Models/eq7.rds")

#####################################
# heterogeneity in signal weight eq8
##################################### 

# credibility interaction
eq8_1 <- brm(
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

summary(eq8_1) ## quick diagnostics 

mcmc_trace(eq8_1)

pp_check(eq8_1) + xlim(-60, 60)

saveRDS(eq8_1, "Models/eq8_1.rds")  

# treatment interaction
eq8_2 <- brm(
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

summary(eq8_2) ## quick diagnostics

mcmc_trace(eq8_2)

pp_check(eq8_2) + xlim(-60, 60)

saveRDS(eq8_2, "Models/eq8_2.rds") 

# tenure interaction 

df |> 
  group_by(tenure_d) |> 
  summarise(n = n()) |> 
  mutate(prop = n / sum(n))

eq8_3 <- brm(
  update ~ 0 + e_prior*tenure_d,
  data = df,
  family = student(),
  prior = c(
    prior(normal(0, 12.07), class = "b"),
    prior(exponential(12.07), class = "sigma")
  ),
  iter = 2500,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 6582) 

summary(eq8_3) ## quick diagnostics 

mcmc_trace(eq8_3) 

pp_check(eq8_3) + xlim(-60, 60) 

saveRDS(eq8_3, "Models/eq8_3.rds")

# are house owners more uncertain? 
df_prior |> 
  mutate(tenure_d = case_when(
    tenure == 1 ~ 0,
    tenure == 2 ~ 1,
    tenure == 3 ~ 1,
    tenure == 4 ~ NA
  )) |> 
  group_by(tenure_d) |> 
  summarise(prior = mean(sd)) 

# LR scale interaction 

# interaction 
eq8_4 <- brm(
  update ~ 0 + e_prior*lr_scale_d,
  data = df,
  family = student(),
  prior = c(
    prior(normal(0, 12.07), class = "b"),
    prior(exponential(12.07), class = "sigma")
  ),
  iter = 2500,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 6582) 

summary(eq8_4) ## quick diagnostics 

mcmc_trace(eq8_4) 

pp_check(eq8_4) + xlim(-60, 60) 

saveRDS(eq8_4, "Models/eq8_4.rds")

# education interaction 

eq8_5 <- brm(
  update ~ 0 + e_prior*educ_d,
  data = df,
  family = student(),
  prior = c(
    prior(normal(0, 12.07), class = "b"),
    prior(exponential(12.07), class = "sigma")
  ),
  iter = 2500,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 6582) 

summary(eq8_5) ## quick diagnostics

mcmc_trace(eq8_5)

pp_check(eq8_5) + xlim(-60, 60)

saveRDS(eq8_5, "Models/eq8_5.rds")

###################################
# eq9
###################################

eq9_1 <- brm(data = df, 
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

summary(eq9_1) ## quick diagnostics

mcmc_trace(eq9_1)

pp_check(eq9_1) + xlim(-60, 60)

saveRDS(eq9_1, "Models/eq9_1.rds")

eq9_2 <- brm(data = df_prior, 
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

summary(eq9_2) ## quick diagnostics

mcmc_trace(eq9_2)

pp_check(eq9_2) + xlim(-60, 60)

saveRDS(eq9_2, "Models/eq9_2.rds")

##################################################################
# Instrumental variable eq10
################################################################## 

sd(df$update_dev, na.rm = TRUE)*.15

stage1_1 <- bf(update ~ 1 + treat) 
stage2_2 <- bf(update_dev ~ 1 + update) 

# differenced 0 prior 
eq10_1 <- brm(data = df, 
           family = gaussian,
           stage1_1 + stage2_2 + set_rescor(TRUE),
           prior = c(
             # first stage
             prior(normal(0, 12.07), class = Intercept, resp = update),
             prior(normal(0, 12.07), class = b, resp = update),
             prior(exponential(12.07), class = sigma, resp = update),
             
             # second stage 
             prior(normal(0, 0.25), class = Intercept, resp = updatedev),
             prior(normal(0, 0.25), class = b, resp = updatedev),
             prior(exponential(0.25), class = sigma, resp = updatedev),
             
             # rho
             prior(lkj(2), class = rescor)),
           
           iter = 2000, 
           warmup = 1000, 
           chains = 4, 
           cores = 4,
           seed = 14)

saveRDS(eq10_1, "Models/eq10_1.rds") 

newdata <- data.frame(update = -57.1)

# Predict updatedev with 95% credible intervals
fitted(eq10_1, 
       newdata = -57.1, 
       resp = "updatedev", 
       re_formula = NA,  # to exclude group-level effects (if present)
       summary = TRUE, 
       probs = c(0.025, 0.975))

# differenced .15 smd prior 
eq10_2 <- brm(data = df, 
           family = gaussian,
           stage1_1 + stage2_2 + set_rescor(TRUE),
           prior = c(
             # first stage
             prior(normal(0, 12.07), class = Intercept, resp = update),
             prior(normal(0, 12.07), class = b, resp = update),
             prior(exponential(12.07), class = sigma, resp = update),
             
             # second stage 
             prior(normal(0, 0.25), class = Intercept, resp = updatedev),
             prior(normal(0.1, 0.25), class = b, resp = updatedev),
             prior(exponential(0.25), class = sigma, resp = updatedev),
             
             # rho
             prior(lkj(2), class = rescor)),
           
           iter = 2000, 
           warmup = 1000, 
           chains = 4, 
           cores = 4,
           seed = 14)

saveRDS(eq10_2, "Models/eq10_2.rds") 


##################################################################
# Instrumental variable interaction
################################################################## 
sd(df$update_prox, na.rm = TRUE)*.15

stage1_2 <- bf(update ~ 1 + treat) 
stage2_2 <- bf(update_prox ~ 1 + update*as.factor(dist)) 

eq11_1 <- brm(data = df, 
               family = gaussian,
               stage1_2 + stage2_2 + set_rescor(TRUE),
               prior = c(
                 # first stage
                 prior(normal(0, 12.07), class = Intercept, resp = update),
                 prior(normal(0, 12.07), class = b, resp = update),
                 prior(exponential(12.07), class = sigma, resp = update),
                 
                 # second stage 
                 prior(normal(0, 0.69), class = Intercept, resp = updateprox),
                 prior(normal(0, 0.69), class = b, resp = updateprox),
                 prior(exponential(0.69), class = sigma, resp = updateprox),
                 
                 # rho
                 prior(lkj(2), class = rescor)),
               
               iter = 2000, 
               warmup = 1000, 
               chains = 4, 
               cores = 4,
               seed = 14) 

saveRDS(eq11_1, "Models/eq11_1.rds") 

dist_levels <- sort(unique(df$dist))  

newdata <- data.frame(update = -57.1,
                      dist = dist_levels)

fitted(eq11_1,
       newdata = newdata,
       resp = "updateprox",
       re_formula = NA,
       summary = TRUE,
       probs = c(0.025, 0.975))

eq11_2 <- brm(data = df, 
              family = gaussian,
              stage1_2 + stage2_2 + set_rescor(TRUE),
              prior = c(
                # first stage
                prior(normal(0, 12.07), class = Intercept, resp = update),
                prior(normal(0, 12.07), class = b, resp = update),
                prior(exponential(12.07), class = sigma, resp = update),
                
                # second stage 
                prior(normal(0, 0.69), class = Intercept, resp = updateprox),
                prior(normal(.1, 0.69), class = b,  coef = "update:as.factordist500", resp = updateprox),
                prior(normal(0, 0.69), class = b, resp = updateprox),
                prior(exponential(0.69), class = sigma, resp = updateprox),
                
                
                # rho
                prior(lkj(2), class = rescor)),
              
              iter = 2000, 
              warmup = 1000, 
              chains = 4, 
              cores = 4,
              seed = 14) 

saveRDS(eq11_2, "Models/eq11_2.rds") 