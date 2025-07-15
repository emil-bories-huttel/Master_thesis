library(tidyverse)

load("data/cleaned_data.RData")

df_likert <- df |> 
  filter(!is.na(likert_change_prior), 
         likert_change_post != "NA") |>
  select(likert_change_prior, likert_change_post) |> 
  pivot_longer(cols = everything(), 
               names_to = "timepoint", 
               values_to = "likert_response")

# Step 2: Count and calculate percentage
df_plot <- df_likert |> 
  group_by(timepoint, likert_response) |> 
  summarise(n = n(), .groups = "drop") |> 
  group_by(timepoint) |> 
  mutate(percentage = n / sum(n) * 100,
         percentage = ifelse(timepoint == "likert_change_prior", percentage, -percentage))  # Mirror 


# Step 3: Plot it
ggplot(df_plot, aes(x = as.factor(likert_response), y = percentage, fill = timepoint)) +
  geom_bar(stat = "identity", position = "identity", width = .8) +
  scale_y_continuous(labels = abs, limits = c(-50, 50)) +
  scale_fill_manual(values = c("likert_change_prior" = "#B06767", 
                               "likert_change_post" = "#911C1C"),
                    labels = c("Post", "Pre")) +
  labs(x = "", y = "Percentage", fill = "") + 
  scale_x_discrete(labels = c("1" = "Large decrease", 
                              "2" = "", 
                              "3" = "", 
                              "4" = "No change", 
                              "5" = "", 
                              "6" = "",
                              "7" = "Large increase")) +
  ## dotted
  #coord_flip() + 
  guides(y = guide_axis(minor.ticks = TRUE), 
         fill  = guide_legend(position = "inside")) + 
  theme(legend.position.inside = c(0.2, 0.85), 
        legend.key.size = unit(0.1, 'cm'))  
  
## % skeptical 
df |> 
  summarise(percent = sum(likert_change_prior >= 4, na.rm = T) / n() * 100)

df |> 
  summarise(percent = sum(likert_change_post >= 4, na.rm = T) / n() * 100)


####################################### 
# Some data vizualization 
#######################################  

## monotonicity check (show number of cases where e_abs_post > e_abs_prior)
df |> 
  filter(e_abs_post > e_abs_prior) |> 
  nrow()

## by treatment arm
df |> 
  filter(e_abs_post > e_abs_prior) |> 
  group_by(treat) |> 
  summarise(n = n())

## mean error adjustment (monotonicity check) 
df |> 
  group_by(treat) |> 
  summarise(mean = mean(e_abs_post - e_abs_prior, na.rm = TRUE)) |> 
  mutate(mean = round(mean, 2)) 

## error adjustment plot (Monotonicity check)
df |>  
  ggplot(aes(x = e_abs_post, y = e_abs_prior, color = as.factor(treat))) + 
  geom_point(shape = 21) + 
  geom_abline(slope = 1, intercept = 0, color = "black") +
  scale_color_manual(values = c("grey30", "#911C1C")) +
  labs(x = "Post-treatment error", y = "Pre-treatment error") + 
  facet_wrap(~treat, labeller = as_labeller(c(`0` = "T1", `1` = "T2"))) + 
  theme_bw() + 
  theme(legend.position = "none")

## 
df |> 
  filter(y1i_post > y1i_prior) |> 
  nrow() 

## belief update (pre-post)
df_long1 <- df |> 
  mutate(id = row_number()) |> 
  select(treat, y1i_prior, y1i_post, e_prior, pro_dev_prior, pro_dev_post, id) |> 
  pivot_longer(cols = c(y1i_prior, y1i_post), names_to = "time", values_to = "y") |> 
  mutate(time = case_when(
    time == "y1i_prior" ~ 0,
    time == "y1i_post" ~ 1
  )) 

df_long2 <- df |> 
  mutate(id = row_number()) |> 
  select(pro_dev_prior, pro_dev_post) |> 
  pivot_longer(cols = c(pro_dev_prior, pro_dev_post), names_to = "time", values_to = "y_dev") |> 
  mutate(time = case_when(
    time == "pro_dev_prior" ~ 0,
    time == "pro_dev_post" ~ 1
  )) 

#merge 
df_long <- cbind(df_long1, df_long2) 

lm(y ~ time*e_prior, data = df_long) |> 
  summary() 

df_long |> 
  lm(formula = y ~ treat*time*e_prior) |> 
  summary()

df |> 
  lm(formula = y1i_post ~ treat*y1i_prior) |> 
  summary()

df |> 
  lm(formula = update ~ treat*e_prior) |> 
  summary()

df |> 
  lm(formula = y1i_post ~ 0 + signal + y1i_prior) |> 
  summary()

df |> 
  lm(formula = y1i_post ~ treat*y1i_prior) |> 
  plot_cme(variables = "treat", condition = "y1i_prior") +
  theme_bw() + 
  ## raw data 
  geom_rug(data = df, aes(x = y1i_prior)) 

## plot marginal effects using marginal effects package

df_long1 |> 
  lm(formula = y ~ time*e_prior) |> 
  plot_cme(variables = "time", condition = "e_prior") +
  theme_bw() + 
  ## raw data 
  geom_rug(data = df_long1, aes(x = e_prior)) 




## error update (pre-post)
df_long <- df |> 
  mutate(id = row_number()) |> 
  select(treat, e_abs_prior, e_abs_post, id) |> 
  pivot_longer(cols = c(e_abs_prior, e_abs_post), names_to = "time", values_to = "y") |> 
  mutate(time = case_when(
    time == "e_abs_prior" ~ 0,
    time == "e_abs_post" ~ 1
  )) 

lm(y ~ time + as.factor(id), data = df_long) |> 
  summary() 

##  update_dev pre-post
df_long <- df |> 
  mutate(id = row_number()) |> 
  select(treat, pro_dev_prior, pro_dev_post, id) |> 
  pivot_longer(cols = c(pro_dev_prior, pro_dev_post), names_to = "time", values_to = "y") |> 
  mutate(time = case_when(
    time == "pro_dev_prior" ~ 0,
    time == "pro_dev_post" ~ 1
  )) 

lm(y ~ time + as.factor(id), data = df_long) |>
  summary() 

## treatment update 
df |> 
  mutate(update = y1i_post - y1i_prior) |>
  lm(formula = update ~ treat + as.factor(tenure) + lr_scale) |>
  summary() 

## treatment error 
df |> 
  lm(formula = e_abs_diff ~ treat) |>
  summary() 

## distribution of update by treatment group 
df |> 
  mutate(update = y1i_post - y1i_prior) |>
  ggplot(aes(x = update, fill = as.factor(treat))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(x = "Update", y = "Count") +
  theme_bw() +
  scale_fill_manual(values = c("grey30", "#911C1C"), 
                    name = "Treatment", labels = c("T1", "T2")) 
  

## distribution of error by treatment group (absolute)
df |> 
  ggplot(aes(x = e_abs_diff, fill = as.factor(treat))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(x = "Update", y = "Count") +
  theme_bw() +
  scale_fill_manual(values = c("grey", "#911C1C")) 

## credibility by group 
df |> 
  group_by(treat) |>
  summarise(mean = mean(credibility, na.rm = TRUE), 
            sd = sd(credibility, na.rm = TRUE))

## 2sls (quick glance)
library(ivreg)

ivreg(update_dev ~ update | treat, data = df) |> 
  summary()

