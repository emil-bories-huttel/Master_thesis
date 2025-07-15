load("data/cleaned_data.RData") 
load("data/df_prior.RData")

library(tidybayes)
library(tidyverse)
library(brms)
library(ggdist)
library(bayesplot)
library(patchwork)
library(modelr)

##############
# theme 
##############

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
  select(pro_dev_prior, pro_dev_post, id, treat) |> 
  pivot_longer(cols = c(pro_dev_prior, pro_dev_post), names_to = "time", values_to = "y_dev") |> 
  mutate(time = case_when(
    time == "pro_dev_prior" ~ 0,
    time == "pro_dev_post" ~ 1
  )) 

############################
# Load models
############################

# eq5 models

eq5_1 <- readRDS("Models/eq5_1.rds")

eq5_2 <- readRDS("Models/eq5_2.rds")

eq5_3 <- readRDS("Models/eq5_3.rds")

eq5_4 <- readRDS("Models/eq5_4.rds")

# eq6 models

eq6_1 <- readRDS("Models/eq6_1.rds")

eq6_2 <- readRDS("Models/eq6_2.rds")

eq6_3 <- readRDS("Models/eq6_3.rds")

eq6_4 <- readRDS("Models/eq6_4.rds")

# eq7 

eq7 <- readRDS("Models/eq7.rds") 

# eq8 

eq8_1 <- readRDS("Models/eq8_1.rds")

eq8_2 <- readRDS("Models/eq8_2.rds")

eq8_3 <- readRDS("Models/eq8_3.rds")

eq8_4 <- readRDS("Models/eq8_4.rds")

eq8_5 <- readRDS("Models/eq8_5.rds") 

# eq9 

eq9_1 <- readRDS("Models/eq9_1.rds") 

eq9_2 <- readRDS("Models/eq9_2.rds") 

# eq10 

eq10_1 <- readRDS("Models/eq10_1.rds")

eq10_2 <- readRDS("Models/eq10_2.rds")

#eq11 

eq11_1 <- readRDS("Models/eq11_1.rds")

eq11_2 <- readRDS("Models/eq11_2.rds")

############################
# plot eq5 
############################

set.seed(274)
# slopes for simple model

eq5_1_draws <- eq5_1 %>%
  spread_draws(b_time, Intercept) |>
  mutate(Intercept = Intercept - 0.5*b_time # uncenter the intercept
  ) |>
  mutate(
    median_intercept = median(Intercept),
    median_slope = median(b_time)
  ) |> 
  ## randomly draw 200 samples for plotting
  sample_n(100) 

median_df1 <- eq5_1_draws |> 
  select(median_intercept, median_slope) |>
  group_by(median_intercept, median_slope) |>
  summarise(median_intercept = median(median_intercept), median_slope = median(median_slope))

eq5.gg1 <- df_long1 |> 
  ggplot(aes(x = time, y = y)) +
  geom_point(color = NA) + 
  # Use geom_segment to specify the start and end points of the lines
  geom_segment(
    aes(x = 0, xend = 1, y = Intercept, yend = Intercept + b_time * 1), 
    data = eq5_1_draws, 
    color = "#B06767", alpha = 0.1
  ) + 
  geom_segment(
    aes(x = 0, xend = 1, y = median_intercept, yend = median_intercept + median_slope * 1), 
    data = median_df1, 
    color = "#911C1C", lwd = .3
  ) +
  ylim(-6, 6) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  geom_vline(xintercept = 1, linetype = "dotted", color = "black") +
  scale_x_continuous(limit = c(-0.1, 1.1), breaks = c(0, 1), labels = c("Pre", "Post")) + 
  labs(
    x = "",
    y = "Expected price change") + 
  facet_wrap(~"Simple pre-post model") + 
  guides(y = guide_axis(minor.ticks = TRUE))

# alt plot showing underlying pre-post distributions
df_long1 |> 
  ggplot(aes(x = time, y = y)) +
  geom_point(color = "#B06767", alpha = .1) + 
  # Use geom_segment to specify the start and end points of the lines
  geom_segment(
    aes(x = -0, xend = 1, y = Intercept, yend = Intercept + b_time * 1), 
    data = eq5_1_draws, 
    color = "#B06767", alpha = 0.1
  ) + 
  geom_segment(
    aes(x = -0, xend = 1, y = median_intercept, yend = median_intercept + median_slope * 1), 
    data = median_df1, 
    color = "#911C1C", lwd = .3
  ) +
  ylim(-50, 50) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  geom_vline(xintercept = 1, linetype = "dotted", color = "black") +
  scale_x_continuous(limit = c(-0.1, 1.1), breaks = c(0, 1), labels = c("Pre", "Post")) + 
  labs(
    x = "",
    y = expression(y[1])) + 
  facet_wrap(~"Simple pre-post model") + 
  guides(y = guide_axis(minor.ticks = TRUE))

# convenience function 

extract_coef <- function(model, type) {
  model %>%
    spread_draws(b_time) |>
    mutate(name = type)
}

# extract time slopes for all models
eq5_1_coef <- extract_coef(eq5_1, "Simple")
eq5_2_coef <- extract_coef(eq5_2, "FE") 
eq5_3_coef <- extract_coef(eq5_3, "RE") 
eq5_4_coef <- extract_coef(eq5_4, "+Cov.")


# combine all models
eq5_coef_df <- bind_rows(eq5_1_coef, eq5_2_coef, eq5_3_coef, eq5_4_coef)

eq5_coef_df$name <- factor(eq5_coef_df$name, levels = c("Simple", "FE", "RE", "+Cov."))

eq5.gg2 <- eq5_coef_df |> 
  ggplot(aes(x = name, y = b_time)) +
  geom_line(aes(group = 1), stat = "summary", fun = median, color = "#911C1C", lwd = .5) + 
  stat_pointinterval(lwd = .7, color = "#911C1C", alpha = .7, size = 3, shape = 21, fill = "white") +
  labs(
    x = "",
    y = expression("Pre-Post diff."~(theta))) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") + 
  scale_y_continuous(position = "right", limits = c(-6,6)) + 
  facet_wrap(~"Coefficient comparison") +
  guides(y = guide_axis(minor.ticks = TRUE)) 

wrap_plots(eq5.gg1, eq5.gg2, nrow = 1) + 
  plot_layout(widths = c(1.5, 1)) 

###################################
# EQ5_4 random intercepts 
###################################

eq5_4_ranef <- eq5_4 %>%
  spread_draws(b_Intercept, r_id[id, term]) %>%
  filter(term == "Intercept") %>%
  mutate(random_intercept = b_Intercept + r_id)

id_order <- eq5_4_ranef %>%
  group_by(id) %>%
  summarise(median_intercept = median(random_intercept)) %>%
  arrange(median_intercept)

eq5_4_ranef$id <- factor(eq5_4_ranef$id, levels = id_order$id)

ranef.gg1 <- eq5_4_ranef |>
  ggplot(aes(x = as.numeric(id), y = random_intercept)) +
  geom_hline(yintercept = 1.32, color = "grey40", lwd = .5) +
  stat_pointinterval(lwd = .7, color = "#911C1C", alpha = .4, size = .1, shape = 21, fill = "white") +
  labs(
    x = "Respondent ID",
    y = "Random intercept ") +
  guides(x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE)) + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  ) + 
  facet_grid("With error"~.) + 
  ylim(-8, 8) 

# just dots
ranef.gg2 <- eq5_4_ranef |> 
  ggplot(aes(x = as.numeric(id), y = random_intercept)) +
  stat_summary(fun = median, geom = "point", color = "#911C1C", size = .8, alpha = 0.8) +
  labs(
    x = "Respondent ID",
    y = "Random intercept"
  ) +
  guides(x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE)) +
  facet_grid("Without error"~.) 

wrap_plots(ranef.gg1, ranef.gg2, nrow = 2) + 
  plot_layout(axis_titles = "collect_y")


####################
# plot eq6
#################### 

set.seed(1654)
#slopes for simple model
eq6_1_draws <- eq6_1 %>%
  spread_draws(b_treat, Intercept) |>
  mutate(Intercept = Intercept - 0.5*b_treat # uncenter the intercept
  ) |>
  mutate(
    median_intercept = median(Intercept),
    median_slope = median(b_treat)
  ) |> 
  ## randomly draw 200 samples for plotting
  sample_n(100) 

median_df2 <- eq6_1_draws |> 
  select(median_intercept, median_slope) |>
  group_by(median_intercept, median_slope) |>
  summarise(median_intercept = median(median_intercept), median_slope = median(median_slope))

eq6.gg1 <- df |> 
  ggplot(aes(x = treat, y = update)) +
  geom_point(color = NA) + 
  # Use geom_segment to specify the start and end points of the lines
  geom_segment(
    aes(x = -0, xend = 1, y = Intercept, yend = Intercept + b_treat * 1), 
    data = eq6_1_draws, 
    color = "#B06767", alpha = 0.1
  ) + 
  geom_segment(
    aes(x = -0, xend = 1, y = median_intercept, yend = median_intercept + median_slope * 1), 
    data = median_df2, 
    color = "#911C1C", lwd = .3
  ) +
  ylim(-6, 6) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  geom_vline(xintercept = 1, linetype = "dotted", color = "black") +
  scale_x_continuous(limit = c(-0.1, 1.1), breaks = c(0, 1), labels = c("T1 (1,9 pct.)", "T2 (7,6 pct)")) + 
  labs(
    x = "",
    y = "Posterior") + 
  facet_wrap(~"Simple treatment model") + 
  guides(y = guide_axis(minor.ticks = TRUE))

# coef 
extract_coef <- function(model, type) {
  model %>%
    spread_draws(b_treat) |>
    mutate(name = type)
} 

# extract time slopes for all models
eq6_1_coef <- extract_coef(eq6_1, "Simple")
eq6_2_coef <- extract_coef(eq6_2, "+Pre")
eq6_3_coef <- extract_coef(eq6_3, "+Cov.")
eq6_4_coef <- extract_coef(eq6_4, "FD")

# combine all models
eq6_coef_df <- bind_rows(eq6_1_coef, eq6_2_coef, eq6_3_coef, eq6_4_coef)

eq6_coef_df$name <- factor(eq6_coef_df$name, levels = c("Simple", "+Pre", "+Cov.", "FD"))

eq6.gg2 <- eq6_coef_df |> 
  ggplot(aes(x = name, y = b_treat)) +
  geom_line(data = eq6_coef_df |> filter(name %in% c("Simple", "+Pre", "+Cov.")),
            aes(group = 1), stat = "summary", fun = median, color = "#911C1C", lwd = .5) + 
  stat_pointinterval(lwd = .7, color = "#911C1C", alpha = .7, size = 3, shape = 21, fill = "white") +
  labs(
    x = "",
    y = expression("Treatment diff. "~(theta))) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") + 
  geom_vline(xintercept = 3.4, color = "black") +
  scale_y_continuous(position = "right", limits = c(-6,6)) + 
  facet_wrap(~"Coefficient comparison") +
  guides(y = guide_axis(minor.ticks = TRUE)) + 
  annotate("text", 
           x = 4, y = 6, 
           label = "Changes", 
           size = 2.5) +
  annotate("text", 
           x = 2, y = 6, 
           label = "Levels", 
           size = 2.5) 

wrap_plots(eq6.gg1, eq6.gg2, nrow = 1) +
  plot_layout(widths = c(1.5, 1)) 


###################################
# plot eq7 
###################################

eq7_draws <- eq7 %>%
  spread_draws(b_y1i_prior, b_signal) |> 
  # long 
  pivot_longer(cols = c(b_y1i_prior, b_signal), names_to = "coef", values_to = "y") |>
  mutate(coef = case_when(
    coef == "b_y1i_prior" ~ "Prior",
    coef == "b_signal" ~ "Signal"
  ))

eq7.gg1 <- eq7_draws |> 
  ggplot(aes(x = coef, y = y)) +
  stat_pointinterval(lwd = .7, color = "#911C1C", alpha = .7, size = 3, shape = 21, fill = "white") +
  #geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.2, fill = "#B06767", color = "#911C1C") + 
  labs(
    x = "",
    y = "") + 
  scale_y_continuous(position = "right", limits = c(0,1)) + 
  coord_flip() + 
  guides(x = guide_axis(minor.ticks = TRUE)) +
  theme(panel.grid.major.y = element_line(color = "grey50", linetype = "dotted", size = 0.5))


eq7_sum_draws <- eq7 %>%
  spread_draws(b_y1i_prior, b_signal) |> 
  mutate(
    sum = b_y1i_prior + b_signal 
  ) |> 
  pivot_longer(cols = c(sum), names_to = "coef", values_to = "y") |> 
  mutate(coef = case_when(
    coef == "sum" ~ "Sum")) 

# sum summary 66 and 95 
eq7_sum_draws |> 
  summarise(
    median = median(y),
    Q2.5 = quantile(y, 0.025),
    Q33 = quantile(y, 0.17),   # Lower bound approx for 66% CI
    Q67 = quantile(y, 0.83),   # Upper bound approx for 66% CI
    Q97.5 = quantile(y, 0.975)
  )

eq7.gg2 <- eq7_sum_draws |>
  ggplot(aes(x = coef, y = y)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey30") +  
  stat_pointinterval(lwd = .7, color = "#911C1C", alpha = .7, size = 3, shape = 21, fill = "white") +
  #geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.2, fill = "#B06767", color = "#911C1C") + 
  labs(
    x = "",
    y = "") + 
  scale_y_continuous(limits = c(.5,1.5)) + 
  coord_flip() + 
  guides(x = guide_axis(minor.ticks = TRUE)) + 
  theme(panel.grid.major.y = element_line(color = "grey50", linetype = "dotted", size = 0.5))

wrap_plots(eq7.gg1, eq7.gg2, nrow = 2) +
  plot_layout(heights = c(2.5, 1)) 

###################################
# plot eq8
###################################

# function to extract predictions 
int_pred <- function(df, model, int_var) {
  new_data <- df |> 
    filter(!is.na({{ int_var }})) |> 
    group_by({{ int_var }}) |> 
    do(data.frame(e_prior = seq(min(.$e_prior), max(.$e_prior), length.out = 100)))
  
  preds <- fitted(model, newdata = new_data, re_formula = NA, probs = c(0.025, 0.975))
  new_data <- bind_cols(new_data, as.data.frame(preds)) 
  
  return(new_data)
}

# extract predictions for credibility 
cred_pred <- int_pred(df, eq8_1, cred_d) 

df_cred <- df |> 
  filter(!is.na(cred_d))

# extract predictions treat
treat_pred <- int_pred(df, eq8_2, treat) 

df_treat <- df |> 
  filter(!is.na(treat)) 

# extract predictions tenure
tenure_pred <- int_pred(df, eq8_3, tenure_d) 

df_tenure <- df |> 
  filter(!is.na(tenure_d)) 

# extract predictions LR 
lr_pred <- int_pred(df, eq8_4, lr_scale_d) 

df_lr <- df |> 
  filter(!is.na(lr_scale_d)) 

# extract predictions for credibility 
educ_pred <- int_pred(df, eq8_5, educ_d) 

df_educ <- df |> 
  filter(!is.na(educ_d)) 

## plotting function 
plot_eq <- function(df1, df2, int_var, lower, upper) {
  df1 %>%
    ggplot(aes(x = e_prior, y = update, color = as.factor({{ int_var }}), group = {{ int_var }})) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "black") + 
    
    # Raw data points
    geom_point(alpha = 0.3, size = .7, color = "#E6CBCB") +
    
    # Model prediction lines
    geom_line(data = df2, 
              aes(x = e_prior, y = Estimate, color = as.factor({{ int_var }})), 
              lwd = 1, alpha = .8) +
    
    # Binned means
    stat_summary_bin(data = df1,
                     aes(x = e_prior, y = update, color = as.factor({{ int_var }}), group = {{ int_var }}),
                     fun = mean, bins = 20, geom = "point", size = 1) +  
    
    # Credible intervals
    geom_ribbon(data = df2, 
                aes(x = e_prior, ymin = Q2.5, ymax = Q97.5, fill = as.factor({{ int_var }})), 
                alpha = 0.2, color = NA, inherit.aes = FALSE) +
    
    scale_color_manual(values = c("#B06767", "#911C1C"),
                       labels = c(lower, upper)) +
    scale_fill_manual(values = c("#B06767", "#911C1C"), 
                      labels = c(lower, upper)) +
    
    labs(
      x = "Signal - Prior",
      y = expression(paste(Delta, " Belief")),
      color = "") +
    ylim(-60, 60) +
    xlim(-60, 60) +
    guides(fill = "none", 
           x = guide_axis(minor.ticks = TRUE),
           y = guide_axis(minor.ticks = TRUE), 
           color  = guide_legend(position = "inside")) + 
    theme(legend.position.inside = c(0.2, 0.85), 
          legend.key.size = unit(0.1, 'cm')) 
}


# mechanisms 
eq8_1
temp <- expression(atop(gamma == 0.42, "CI [0.30, 0.54]")) #extracted from model output

eq8.gg1 <- plot_eq(df_cred, cred_pred, cred_d, lower = "Low", upper = "High") +
  facet_wrap(~"Credibility") + 
  labs(x = "Signal - Prior", 
       y = expression(paste(Delta ~"Belief"))) + 
  annotate("text", 
           x = 40, y = -50, 
           label = temp, 
           size = 2.5) 


eq8_2
temp <- expression(atop(gamma == -0.18, "CI [-0.29, -0.08]"))

eq8.gg2 <- plot_eq(df_treat, treat_pred, treat, lower = "T1", upper = "T2") +
  facet_wrap(~"Treatment") +
  scale_y_continuous(position = "right", limits = c(-60,60)) +
  labs(x = "Signal - Prior", 
       y = "") +
  annotate("text", 
           x = 40, y = -50, 
           label = temp, 
           size = 2.5) 

wrap_plots(eq8.gg1, eq8.gg2, nrow = 1) + 
  plot_layout(axis_titles = "collect_x")

# background

eq8_3
temp <- expression(atop(gamma == 0.21, "CI [0.10, 0.32]"))

eq8.gg3 <- plot_eq(df_tenure, tenure_pred, tenure_d, lower = "Renter", upper = "Owner") +
  facet_wrap(~"Tenure") +
  annotate("text", 
           x = 40, y = -50, 
           label = temp, 
           size = 2.5) 

eq8_4
temp <- expression(atop(gamma == -0.32, "CI [-0.45, -0.17]"))

eq8.gg4 <- plot_eq(df_lr, lr_pred, lr_scale_d, lower = "Left", upper = "Right") +
  facet_wrap(~"Ideology") +   # remove y scale padding
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    strip.text = element_text(),  
    plot.margin = margin(0, 5, 0, -10, unit = "pt")  # negative left margin to shift left
  ) +
  labs(x = "Signal - Prior", y = "") +
  annotate("text", x = 40, y = -50, label = temp, size = 2.5)

eq8_5
temp <- expression(atop(gamma == 0.50, "CI [0.27, 0.63]"))

eq8.gg5 <- plot_eq(df_educ, educ_pred, educ_d, lower = "Other", upper = "Tertiery") +
  facet_wrap(~"Education") + 
  scale_y_continuous(position = "right", limit = c(-60, 60)) + 
  labs(x = "Signal - Prior",  
       y = "")+
  annotate("text", 
           x = 40, y = -50, 
           label = temp, 
           size = 2.5) 

# Combine plots 
wrap_plots(eq8.gg3, eq8.gg4, eq8.gg5, nrow = 1) + 
  plot_layout(axis_titles = "collect_x")

##################################
# eq9
##################################

# eq9_1
credibility_dist <- df %>%
  count(credibility) %>%
  mutate(prop = n / sum(n)) 

posterior <- eq9_1 %>%
  spread_draws(b_e_prior, r_credibility[credibility, term]) %>%
  filter(term %in% c("e_prior")) %>%
  mutate(group_value = case_when(
    term == "e_prior" ~ b_e_prior + r_credibility
  ))

eq9.gg1 <- posterior %>%
  filter(term == "e_prior") %>%
  ggplot(aes(x = as.numeric(credibility), y = group_value)) +
  geom_hline(yintercept = .85, linetype = "dotted", color = "grey40", lwd = .5) +
  geom_hline(yintercept = .65, color = "grey40", lwd = .5) +
  geom_hline(yintercept = .39, linetype = "dotted", color = "grey40", lwd = .5) +
  # Distribution as background bars (scaled to relative height)
  geom_bar(data = credibility_dist, aes(x = as.numeric(credibility), y = prop), 
           stat = "identity", fill = "#E6CBCB", width = 0.4, alpha = 0.3, inherit.aes = FALSE) +
  geom_smooth(method = "loess", se = FALSE, color = "#E6CBCB", lwd = .5) +
  geom_line(aes(group = 1), stat = "summary", fun = median, color = "#911C1C", lwd = .5) + 
  stat_pointinterval(lwd = .7, color = "#911C1C", alpha = .7, size = 3, shape = 21, fill = "white") +
  #stat_slab() + 
  
  # Y-axis labels for effect estimate
  labs(y = expression(pi[s]), x = expression("Credibility Level " ~(sigma[x]^2 ~"proxy"))) +
  guides(y = guide_axis(minor.ticks = TRUE)) +
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  scale_y_continuous(expand = c(0.003, 0), limits = c(-.01, 1.1)) 

sd_dist <- df_prior %>%
  count(sd) %>%
  mutate(prop = n / sum(n)) 

posterior <- eq9_2 %>%
  spread_draws(b_e_prior, r_sd[sd, term]) %>%
  filter(term %in% c("e_prior")) %>%
  mutate(group_value = case_when(
    term == "e_prior" ~ b_e_prior + r_sd
  ))

eq9.gg2 <- posterior %>%
  filter(term == "e_prior") %>%
  ggplot(aes(x = as.numeric(sd), y = group_value)) +
  geom_hline(yintercept = .82, linetype = "dotted", color = "grey40", lwd = .5) +
  geom_hline(yintercept = .63, color = "grey40", lwd = .5) +
  geom_hline(yintercept = .40, linetype = "dotted", color = "grey40", lwd = .5) +
  geom_bar(data = sd_dist, aes(x = as.numeric(sd), y = prop), 
           stat = "identity", fill = "#E6CBCB", width = 0.4, alpha = 0.3, inherit.aes = FALSE) +
  geom_smooth(method = "loess", se = FALSE, color = "#E6CBCB", lwd = .5) +
  geom_line(aes(group = 1), stat = "summary", fun = median, color = "#911C1C", lwd = .5) + 
  stat_pointinterval(lwd = .7, color = "#911C1C", alpha = .7, size = 3, shape = 21, fill = "white") +
  #stat_slab() + 
  
  # Y-axis labels for effect estimate
  labs(y = expression(pi[s]), 
       #x = expression("Pior SD" ~(sqrt(sigma[0]^2)))
       x = expression("Pior SD" ~(hat(sigma)[i]))) +
  guides(y = guide_axis(minor.ticks = TRUE)) +
  scale_x_continuous(breaks = seq(1, 10, 1), 
                     labels = c(
                       "1" = "1",
                       "2" = "2",
                       "3" = "3",
                       "4" = "4",
                       "5" = "5",
                       "6" = "6",
                       "7" = "7",
                       "8" = "8",
                       "9" = "9",
                       "10" = "10+")) + 
  scale_y_continuous(expand = c(0.003, 0), limits = c(-.01, 1.1), position = "right") 

wrap_plots(eq9.gg1, eq9.gg2, nrow = 1)

##################################
# Updating histogram
##################################

df |> 
  ggplot(aes(x = update/e_prior)) +
  #geom_vline(xintercept = c(-.1,1.1), color = "grey30") +
  #shade area betwen 0 and 1 
  geom_rect(aes(xmin = -.1, xmax = 1.1, ymin = -0.01, ymax = 2.1), 
            fill = "grey80", alpha = .2) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 0.1, 
                 fill = "#E6CBCB", 
                 color = "#B06767", 
                 lwd = .3)+
  geom_density(aes(y = ..density..), 
               color = "#911C1C", 
               lwd = .5) + 
  labs(
    #x = expression(paste(Delta, " Belief / Inital Gap")),
    x = expression(paste(pi[si])),
    y = "Density") + 
  guides(x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE)) + 
  scale_x_continuous(limits = c(-6,6)) + 
  scale_y_continuous(expand = c(0.003, 0), limits = c(-.01, 2.1)) +
  annotate("text", 
             x = .5, y = 2, 
             label = "52,3%", 
             size = 2.5) + 
  annotate("text", 
           x = -3, y = 2, 
           label = "18%", 
           size = 2.5) + 
  annotate("text", 
           x = 4.5, y = 2, 
           label = "29,7%", 
           size = 2.5) 

# percent under 0 
df |> 
  mutate(update = update/e_prior) |> 
  summarise(mean = mean(update < 0)) |> 
  mutate(mean = mean * 100)

# pecent between 0 and 1 
df |> 
  mutate(update = update/e_prior) |> 
  summarise(mean = mean(update >= 0 & update <= 1)) |> 
  mutate(mean = mean * 100)

# percent above 1
df |> 
  mutate(update = update/e_prior) |> 
  summarise(mean = mean(update > 1)) |> 
  mutate(mean = mean * 100) 

# percent = 1 
df |> 
  mutate(update = update/e_prior) |> 
  summarise(mean = mean(update == 1)) |> 
  mutate(mean = mean * 100)

# percent = 0 
df |> 
  mutate(update = update/e_prior) |> 
  summarise(mean = mean(update == 0)) |> 
  mutate(mean = mean * 100)

# by treatment group 
df |> 
  ggplot(aes(x = update/e_prior, fill = as.factor(treat), color = as.factor(treat))) +
  #geom_vline(xintercept = c(-.1,1.1), color = "grey30") +
  #shade area betwen 0 and 1 
  geom_histogram(aes(y = ..density..), 
                 binwidth = 0.1, 
                 lwd = .3, 
                 alpha = .7, 
                 position = "identity")+
  labs(
    #x = expression(paste(Delta, " Belief / Inital Gap")),
    x = expression(paste(pi[si])),
    y = "Density", 
    fill = "", 
    color = "") + 
  guides(x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE),
         fill  = guide_legend(position = "inside")) + 
  scale_x_continuous(limits = c(-6,6)) + 
  scale_fill_manual(values = c("grey30", "#911C1C"),
                    labels = c("T1", "T2")) +
  scale_color_manual(values = c("grey30", "#911C1C"),
                    labels = c("T1", "T2")) + 
  theme(legend.position.inside = c(0.8, 0.85), 
        legend.key.size = unit(0.1, 'cm'))

df |> 
  filter(!is.na(lr_scale_d)) |>
  ggplot(aes(x = update/e_prior, fill = as.factor(lr_scale_d), color = as.factor(lr_scale_d))) +
  #geom_vline(xintercept = c(-.1,1.1), color = "grey30") +
  #shade area betwen 0 and 1 
  geom_histogram(aes(y = ..density..), 
                 binwidth = 0.1, 
                 lwd = .3, 
                 alpha = .7, 
                 position = "identity")+
  labs(
    #x = expression(paste(Delta, " Belief / Inital Gap")),
    x = expression(paste(pi[si])),
    y = "Density", 
    fill = "", 
    color = "") + 
  guides(x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE),
         fill  = guide_legend(position = "inside")) + 
  scale_x_continuous(limits = c(-6,6)) + 
  scale_fill_manual(values = c("grey30", "#911C1C"),
                    labels = c("Left", "Right")) +
  scale_color_manual(values = c("grey30", "#911C1C"),
                     labels = c("Left", "Right")) + 
  theme(legend.position.inside = c(0.8, 0.85), 
        legend.key.size = unit(0.1, 'cm'))

##################################
# eq10 
##################################

draws_eq10_1 <- df %>%
  data_grid(update = seq_range(update, n = 100)) %>%
  add_epred_draws(eq10_1, resp = "updatedev", ndraws = 100, seed = 342)

draws_all_1 <- df %>%
  data_grid(update = seq_range(update, n = 100)) %>%
  add_epred_draws(eq10_1, resp = "updatedev")  # full posterior

eq10.gg1 <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey30") +
  geom_line(
    data = draws_eq10_1,
    aes(x = update, y = .epred, group = .draw),
    alpha = 0.1,
    color = "#B06767"
  ) +
  stat_summary(
    data = draws_all_1,
    aes(x = update, y = .epred),
    fun = median,
    geom = "line",
    color = "#911C1C",
    size = .7
  ) +
  labs(
    x = expression(paste(Delta, " Belief")),
    y = expression("Predicted " ~Delta ~"Preferences"),
  ) + 
  geom_rug(data = df, aes(x = update), sides = "b", color = "grey30", alpha = 0.3) +
  facet_wrap(~"Centered prior") +
  guides(x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE)) +
  ylim(-6,6)



draws_eq10_2 <- df %>%
  data_grid(update = seq_range(update, n = 100)) %>%
  add_epred_draws(eq10_2, resp = "updatedev", ndraws = 100, seed = 654)


draws_all_2 <- df %>%
  data_grid(update = seq_range(update, n = 100)) %>%
  add_epred_draws(eq10_2, resp = "updatedev")  # full posterior


eq10.gg2 <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey30") +
  geom_line(
    data = draws_eq10_2,
    aes(x = update, y = .epred, group = .draw),
    alpha = 0.1,
    color = "#B06767"
  ) +
  stat_summary(
    data = draws_all_2,
    aes(x = update, y = .epred),
    fun = median,
    geom = "line",
    color = "#911C1C",
    size = .7
  ) +
  labs(
    x = expression(paste(Delta, " Belief")),
    y = "",
  ) + 
  # add rug from df$update
  geom_rug(data = df, aes(x = update), sides = "b", color = "grey30", alpha = 0.3) +
  facet_wrap(~"Directional prior") + 
  scale_y_continuous(position = "right", limits = c(-6,6)) + 
  guides(x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE)) 

wrap_plots(eq10.gg1, eq10.gg2, ncol = 2) + 
  plot_layout(axis_titles = "collect_x")

################################## 
# eq11 
##################################

# Extract and transform draws from eq11_1
eq11_1_draws <- eq11_1 |> 
  spread_draws(b_updateprox_update, 
               `b_updateprox_update:as.factordist500`,
               `b_updateprox_update:as.factordist1000`, 
               `b_updateprox_update:as.factordist3000`,
               `b_updateprox_update:as.factordist5000`) |> 
  mutate(marginal_dist80 = b_updateprox_update,
         marginal_dist500  = b_updateprox_update + `b_updateprox_update:as.factordist500`,
         marginal_dist1000 = b_updateprox_update + `b_updateprox_update:as.factordist1000`,
         marginal_dist3000 = b_updateprox_update + `b_updateprox_update:as.factordist3000`,
         marginal_dist5000 = b_updateprox_update + `b_updateprox_update:as.factordist5000`) |> 
  pivot_longer(
    cols = starts_with("marginal"),
    names_to = "marginal_dist",
    values_to = "marginal_dist_value"
  ) |> 
  mutate(marginal_dist = str_remove(marginal_dist, "marginal_dist"), 
         marginal_dist = as.numeric(marginal_dist)/1000, 
         prior = "Centered",
         model = "eq11_1") 

# Extract and transform draws from eq11_2
eq11_2_draws <- eq11_2 |> 
  spread_draws(b_updateprox_update, 
               `b_updateprox_update:as.factordist500`,
               `b_updateprox_update:as.factordist1000`, 
               `b_updateprox_update:as.factordist3000`,
               `b_updateprox_update:as.factordist5000`) |> 
  mutate(marginal_dist80 = b_updateprox_update,
         marginal_dist500  = b_updateprox_update + `b_updateprox_update:as.factordist500`,
         marginal_dist1000 = b_updateprox_update + `b_updateprox_update:as.factordist1000`,
         marginal_dist3000 = b_updateprox_update + `b_updateprox_update:as.factordist3000`,
         marginal_dist5000 = b_updateprox_update + `b_updateprox_update:as.factordist5000`) |> 
  pivot_longer(
    cols = starts_with("marginal"),
    names_to = "marginal_dist",
    values_to = "marginal_dist_value"
  ) |> 
  mutate(marginal_dist = str_remove(marginal_dist, "marginal_dist"), 
         marginal_dist = as.numeric(marginal_dist)/1000, 
         prior = "Directional",
         model = "eq11_2") 

# Combine both models into one data frame
combined_draws <- bind_rows(eq11_1_draws, eq11_2_draws)

# Compute median marginal effects for each model and distance
combined_medians <- combined_draws %>%
  group_by(prior, marginal_dist) %>%
  summarise(median = median(marginal_dist_value))

# Plot the combined data
combined_draws |> 
  ggplot(aes(x = marginal_dist, y = marginal_dist_value, color = prior)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_line(data = combined_medians, aes(y = median), lwd = 0.6) +
  stat_pointinterval(aes(group = prior), 
                     position = position_dodge(width = 0.2),
                     lwd = 1, 
                     alpha = 0.7, 
                     size = 2.5, 
                     shape = 21, 
                     fill = "white") +
  scale_x_continuous(breaks = c(0.08, 0.5, 1, 3, 5), labels = c("0.08" = ".08",
                                                                "0.5" = ".5",
                                                                "1" = "1",
                                                                "3" = "3",
                                                                "5" = "5")) + 
  scale_color_manual(values = c("#911C1C", "#B06767")) +
  labs(x = "Distance to project (km)", 
       y = "Marginal change in preferences", 
       color = "") + 
  ylim(c(-.20, .20)) + 
  guides(y = guide_axis(minor.ticks = TRUE),
         color  = guide_legend(position = "inside")) + 
  theme(legend.position.inside = c(0.8, 0.85), 
        legend.key.size = unit(0.1, 'cm')) 

# nr of n in dist 
df |> 
  group_by(as.factor(dist)) |>
  summarise(n = n(), 
            mean1 = mean(pro_dev_prior, na.rm = TRUE),
            mean2 = mean(pro_dev_post, na.rm = TRUE))

df |> 
  group_by(as.factor(region)) |>
  summarise(n = n(), 
            mean1 = mean(pro_dev_prior, na.rm = TRUE),
            mean2 = mean(pro_dev_post, na.rm = TRUE))

df |> 
  group_by(as.factor(dist)) |>
  summarise(n = n(), 
            mean1 = mean(proximity_prior, na.rm = TRUE),
            mean2 = mean(proximity_post, na.rm = TRUE))

df |> 
  group_by(as.factor(region)) |>
  summarise(n = n(), 
            mean1 = mean(proximity_prior, na.rm = TRUE),
            mean2 = mean(proximity_post, na.rm = TRUE))


df |> 
  summarise(n = n(), 
            mean1 = mean(proximity_prior, na.rm = TRUE),
            mean2 = mean(pro_dev_prior, na.rm = TRUE))

