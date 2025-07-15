library(readxl)
library(tidyverse)
library(cobalt)
library(brms)
library(marginaleffects)
library(patchwork)


df <- read_excel("data/raw_data_labelled_final.xlsx")

##########################
# Clean and wrangle data #
##########################

## Create treatment indicator (1 if t1=0 and 2 if t2=1)

df <- df |> 
  mutate(treat = case_when(
    t1 == 1 ~ 0,
    t2 == 1 ~ 1,
    TRUE ~ NA_real_
  )) |>  
  relocate(treat, .before = t1)

df <- df |> 
  mutate(signal = case_when(
    t1 == 1 ~ -1.9,
    t2 == 1 ~ -7.6,
    TRUE ~ NA_real_
  ))

df |> 
  select(t1, t2, treat, signal)

## distance treatment if dev80m_prior != NA dist=80, if dev500m_prior != NA dist=500 etc 
df <- df |> 
  mutate(dist = case_when(
    !is.na(dev80m_prior) ~ 80,
    !is.na(dev500m_prior) ~ 500,
    !is.na(dev1k_prior) ~ 1000,
    !is.na(dev3k_prior) ~ 3000,
    !is.na(dev5k_prior) ~ 5000,
    TRUE ~ NA_real_
  )) 

# proximity likerts collapse into one variable  
df <- df |> 
  mutate(
    proximity_prior = coalesce(dev80m_prior, dev500m_prior, dev1k_prior, dev3k_prior, dev5k_prior),
    proximity_post  = coalesce(dev80m_post,  dev500m_post,  dev1k_post,  dev3k_post,  dev5k_post)
  ) |>  
  relocate(proximity_prior, .before = dev80m_prior) |> 
  relocate(proximity_post, .before = dev80m_post)

## FMC1 correct = 0 
## FMC2 correct = 0
# sum of fmc1 and fmc2 (i.e. number of failed attention questions) 
df <- df |> 
  mutate(fmc1_bin = case_when(
    fmc1 == 1 ~ 1,
    fmc1 == 2 ~ 0,
    fmc1 == 3 ~ 1,
  )) |> 
  mutate(fmc2_bin = case_when(
    fmc2 == 1 ~ 1,
    fmc2 == 2 ~ 0,
    fmc2 == 3 ~ 1,
  )) |>  
  mutate(fmc_sum = fmc1_bin + fmc2_bin)



## summary stats of fmc 
df |> 
  group_by(fmc_sum) |>
  summarise(n = n())

df |> 
  group_by(treat, fmc_sum) |>
  summarise(n = n())


## comprehension (if coffee_lb < coffee_mu < coffee_ub = 0) 
df <- df |> 
  mutate(coffee_correct = case_when(
    coffee_lb < coffee_mu & coffee_mu < coffee_ub ~ 0,
    TRUE ~ 1
  )) |> 
  ## comprehension (if y2i_prior < y1i_prior < y3i_prior = 0)
  mutate(prior_correct = case_when(
    y2i_prior < y1i_prior & y1i_prior < y3i_prior ~ 0,
    TRUE ~ 1
  )) |> 
  ## comprehension (if y2i_post < y1i_post < y3i_post)
  mutate(post_correct = case_when(
    y2i_post < y1i_post & y1i_post < y3i_post ~ 0,
    TRUE ~ 1
  )) |>
  ## sum of comprehension questions (i.e. number of Manski question errors)
  mutate(comp_error_sum = coffee_correct + prior_correct + post_correct, 
         p_comp_error_sum = coffee_correct + prior_correct) ## Pre-treatment errors 


### summary stats of comprehension
df |>
  group_by(comp_error_sum) |>
  summarise(n = n())

df |> 
  group_by(treat, comp_error_sum) |>
  summarise(n = n())

df |> 
  group_by(comp_error_sum) |>
  summarise(mean = mean(y1i_prior))


df |> 
  group_by(comp_error_sum) |>
  summarise(mean = mean(y1i_post)) 

## Error pre and post (treat = 1 then - 1,9 and if  treat = 2 then - 7,6)
df <- df |> 
  mutate(e_prior = case_when(
    treat == 0 ~ -1.9 - y1i_prior,
    treat == 1 ~ -7.6 - y1i_prior,
    TRUE ~ NA_real_
  )) |> 
  mutate(e_post = case_when(
    treat == 0 ~ -1.9 - y1i_post,
    treat == 1 ~ -7.6 - y1i_post,
    TRUE ~ NA_real_
  )) |> 
  ## absolute 
  mutate(e_abs_prior = abs(e_prior), 
         e_abs_post = abs(e_post), 
         e_abs_diff = e_abs_post - e_abs_prior)


df |> 
  select(treat, y1i_prior, y1i_post, e_prior, e_post) 

## don't know as NA 
df <- df |> 
  mutate(pro_dev_prior = case_when(
    pro_dev_prior == 8 ~ NA_real_,
    TRUE ~ pro_dev_prior
  )) |>
  mutate(pro_dev_prior = case_when(
    pro_dev_post == 8 ~ NA_real_,
    TRUE ~ pro_dev_prior
  )) |> 
  mutate(pro_dev_post = case_when(
    pro_dev_post == 8 ~ NA_real_,
    TRUE ~ pro_dev_post
  )) |>
  mutate(pro_dev_post = case_when(
    pro_dev_prior == 8 ~ NA_real_,
    TRUE ~ pro_dev_post
  )) |>
  mutate(proximity_prior = case_when(
    proximity_prior == 8 ~ NA_real_,
    TRUE ~ proximity_prior
  )) |> 
  mutate(proximity_post = case_when(
    proximity_post == 8 ~ NA_real_,
    TRUE ~ proximity_post
  )) |> 
  mutate(likert_change_prior = case_when(
    likert_change_prior == 8 ~ NA_real_,
    TRUE ~ likert_change_prior
  )) |>
  mutate(likert_change_post = case_when(
    likert_change_post == 8 ~ NA_real_,
    TRUE ~ likert_change_post
  )) |>
  mutate(educ = case_when(
    educ == 7 ~ NA_real_,
    TRUE ~ educ)) |>
  mutate(sex = case_when(
    sex > 2 ~ NA_real_,
    TRUE ~ sex)) |>
  mutate(urban = case_when(
    urban == 10 ~ NA_real_,
    TRUE ~ urban)) |>
  mutate(tenure = case_when(
    tenure == 5 ~ NA_real_,
    TRUE ~ tenure)) |> 
  mutate(credibility = case_when(
    credibility == 8 ~ NA_real_,
    TRUE ~ credibility)) 

df <- df |> 
  mutate(urban5 = case_when(
    urban == 1 ~ 1,                          # København
    urban == 2 ~ 2,                          # Aarhus/Aalborg/Odense
    urban %in% 3:5 ~ 3,                      # Cities/towns over 10,000
    urban %in% 6:8 ~ 4,                      # Small towns
    urban == 9 ~ 5,                          # Rural
    TRUE ~ NA_real_
  ))

## reverse likert scale 7 = large increase 
df <- df |>
  mutate(likert_change_prior = case_when(
    likert_change_prior == 1 ~ 7,
    likert_change_prior == 2 ~ 6,
    likert_change_prior == 3 ~ 5,
    likert_change_prior == 4 ~ 4,
    likert_change_prior == 5 ~ 3,
    likert_change_prior == 6 ~ 2,
    likert_change_prior == 7 ~ 1,
    TRUE ~ NA_real_
  )) |>
  mutate(likert_change_post = case_when(
    likert_change_post == 1 ~ 7,
    likert_change_post == 2 ~ 6,
    likert_change_post == 3 ~ 5,
    likert_change_post == 4 ~ 4,
    likert_change_post == 5 ~ 3,
    likert_change_post == 6 ~ 2,
    likert_change_post == 7 ~ 1,
    TRUE ~ NA_real_
  ))


## updating 
df <- df |> 
  mutate(update = y1i_post - y1i_prior, 
         update_dev = pro_dev_post - pro_dev_prior, 
         update_prox = proximity_post - proximity_prior)

#  sex 0-1 dummy 
df <- df |> 
  mutate(sex = if_else(
    sex == 1, 0, 1 
  ))

df$dev_binary0 <- ifelse(df$pro_dev_prior >= 4, 1, 0)

df$dev_binary1 <- ifelse(df$pro_dev_post >= 4, 1, 0)

# cred dummy
df <- df |> 
  mutate(cred_d = case_when(
    credibility <= 4 ~ 0,
    credibility > 4 ~ 1
  )) 

# tenure dummy 
df <- df |> 
  mutate(tenure_d = case_when(
    tenure == 1 ~ 0,
    tenure == 2 ~ 1,
    tenure == 3 ~ 1,
    tenure == 4 ~ NA
  )) 

# LR scale dummy 
df <- df |> 
  mutate(lr_scale_d = case_when(
    lr_scale == 1 ~ 0,
    lr_scale == 2 ~ 0,
    lr_scale == 3 ~ 0,
    lr_scale == 4 ~ 0,
    lr_scale == 5 ~ NA,
    lr_scale == 6 ~ 1,
    lr_scale == 7 ~ 1,
    lr_scale == 8 ~ 1,
    lr_scale == 9 ~ 1,
    lr_scale == 10 ~ 1,
  ))

#education dummy 
df <- df |> 
  mutate(educ_d = ifelse(educ <= 3, 0, 1))

# amount in group 
df |> 
  group_by(educ_d) |> 
  summarise(n = n()) |> 
  mutate(prop = n / sum(n)) 

# anti-development dummy 
df <- df |> 
  mutate(pro_dev_post_d = ifelse(pro_dev_post >= 4, 0, 1),
         pro_dev_prior_d = ifelse(pro_dev_prior >= 4, 0, 1),
         update_dev_d = pro_dev_post_d - pro_dev_prior_d) 

df <- df |> 
  mutate(prox_post_d = ifelse(proximity_post >= 4, 0, 1),
         prox_prior_d = ifelse(proximity_prior >= 4, 0, 1),
         update_prox_d = prox_post_d - prox_prior_d) 

## Drop 2/3 wrong answers on pre-treatment comprehension questions 
df_fail <- df # keep for error figs 

df <- df |> 
  filter(p_comp_error_sum < 2) 


####################################
# summary stats of background vars # 
#################################### 

## these are used for table 2 

# percentage in agegroup 
df <- df |> 
  mutate(age = 2025 - year) |> 
  mutate(agegroup = case_when(
    age < 30 ~ "18-29",
    age >= 30 & age < 40 ~ "30-39",
    age >= 40 & age < 50 ~ "40-49",
    age >= 50 & age < 60 ~ "50-59",
    age >= 60 & age < 70 ~ "60-69",
    age >= 60 ~ "70+"
  )) 

## summary stats of agegroup
df |> 
  filter(agegroup != "NA") |>
  group_by(agegroup) |>
  summarise(n = n()) |> 
  mutate(percentage = n / sum(n) * 100) |> 
  select(-n) 

## summary stats educ 
df |> 
  filter(educ != 7) |>
  group_by(educ) |>
  summarise(n = n()) |> 
  mutate(percentage = n / sum(n) * 100) |> 
  select(-n) 

## summary stats gender 
df |>
  filter(sex != "NA") |> 
  group_by(sex) |> 
  summarise(n = n()) |>
  mutate(percentage = n / sum(n) * 100) |>
  select(-n)
  

### summary stats region 
df |>
  filter(region != "NA") |>
  group_by(region) |>
  summarise(n = n()) |> 
  mutate(percentage = n / sum(n) * 100) |> 
  select(-n) 

## 
df |> 
  group_by(region) |>
  summarise(mean = mean(y1i_prior))

#######################################################
##### Covariate balance table treatment (table 3) #####
#######################################################

bal.tab(as.factor(treat) ~ as.factor(educ) + as.factor(region) + as.factor(sex) + 
          as.factor(tenure) + as.factor(urban5) + y1i_prior + proximity_prior + 
          pro_dev_prior + age + lr_scale + credibility + fmc_sum + e_prior + 
          comp_error_sum + likert_change_prior + y1i_post + pro_dev_post + 
          likert_change_post + e_post + 
          update + update_dev, 
        data = df, 
        disp = c("means")) 

## Outcomes only
df |> 
  lm(formula = y1i_post ~ treat) |>
  summary()

df |>
  lm(formula = update ~ treat) |>
  summary()

df |> 
  lm(formula = update_dev ~ treat) |>
  summary() 

df |> 
  lm(formula = pro_dev_post ~ treat) |>
  summary()

df |> 
  lm(formula = likert_change_post ~ treat) |>
  summary()

df_long_b |> 
  lm(formula = y ~ time) |>
  summary()

-3.49/sd(df_long_b$y, na.rm = T)

df_long_p |> 
  lm(formula = y_dev ~ time) |>
  summary()

-0.20/sd(df_long_p$y_dev, na.rm = T)
  
#########################################################
# Rounding, FMC and comprehension check (appendix fig)
#########################################################

#rounding

p5 <- df |>
  mutate(last_digit = (abs(y1i_prior) * 10) %% 10 |> floor()) |>
  ggplot(aes(x = factor(last_digit))) +
  geom_bar(fill = "#911C1C", color = "white") +
  labs(x = "Last digit of y1", y = "Frequency") +
  theme(legend.position = "none") +
  ylim(0, 150)

p6 <- df |>
  mutate(last_digit = (abs(y1i_post) * 10) %% 10 |> floor()) |>
  ggplot(aes(x = factor(last_digit))) +
  geom_bar(fill = "#911C1C", color = "white") +
  labs(x = "Last digit of y1 posterior", y = "Frequency") +
  theme(legend.position = "none") +
  ylim(0, 150)

# anchoring 
p3 <- df |> 
  ggplot(aes(x = y1i_prior)) +
  geom_histogram(binwidth = 2, fill = "#B06767", color = "white") +
  geom_density(aes(y = ..count..), color = "#911C1C", lwd = 0.5) +
  labs(x = "y1 prior", y = "Frequency") +
  theme(legend.position = "none") + 
  guides(x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE)) +
  scale_x_continuous(limits = c(-50,50), breaks = seq(-50, 50, by = 20)) + 
  ylim(0, 100)

p4 <- df |> 
  ggplot(aes(x = y1i_post)) +
  geom_histogram(binwidth = 2, fill = "#B06767", color = "white", width = 10) +
  geom_density(aes(y = ..count..), color = "#911C1C", lwd = 0.5) +
  labs(x = "y1 posterior", y = "Frequency") +
  theme(legend.position = "none") + 
  guides(x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE)) +
  geom_vline(xintercept = -7.6, linetype = "dotted", color = "black") +
  geom_vline(xintercept = -1.9, linetype = "dotted", color = "black") +
  scale_x_continuous(limits = c(-50,50), breaks = seq(-50, 50, by = 20)) +
  ylim(0, 100)

# table of bin frequencies round to nearest whole number
df |> 
  mutate(y1i_prior = round(y1i_prior)) |> 
  group_by(y1i_prior) |>
  summarise(n = n()) |> 
  mutate(percentage = n / sum(n) * 100) |> 
  select(-n) |> 
  arrange(y1i_prior) |> 
  print(n = 100)


## bar chart comprehension errors 
p1 <- df_fail |> 
  group_by(comp_error_sum) |>
  summarise(n = n()) |> 
  mutate(percentage = n / sum(n) * 100) |> 
  select(-n) |> 
  ggplot(aes(x = as.factor(comp_error_sum), y = percentage)) + 
  geom_bar(stat = "identity", fill = "#911C1C", width = .5) + 
  labs(x = "Comprehension errors", y = "Percentage") + 
  theme(legend.position = "none") + 
  scale_x_discrete(labels = c("0", "1", "2", "3")) +
  guides(y = guide_axis(minor.ticks = TRUE)) + 
  ylim(0, 80) 

## bar chart fmc 
p2 <- df_fail |> 
  group_by(fmc_sum) |>
  summarise(n = n()) |> 
  mutate(percentage = n / sum(n) * 100) |> 
  select(-n) |> 
  ggplot(aes(x = as.factor(fmc_sum), y = percentage)) + 
  geom_bar(stat = "identity", fill = "#911C1C", width = .5) + 
  labs(x = "FMC errors", y = " ") + 
  theme(legend.position = "none") + 
  scale_x_discrete(labels = c("0", "1", "2")) + 
  guides(y = guide_axis(minor.ticks = TRUE)) + 
  scale_y_continuous(position = "right", limits = c(0,80))



wrap_plots(p1, p2, nrow = 1) 

wrap_plots(p3, p4, p5, p6, nrow = 2)


##################################################
## Export cleaned data for subsequent analysis
##################################################

save(df, file = "data/cleaned_data.RData")

 

        