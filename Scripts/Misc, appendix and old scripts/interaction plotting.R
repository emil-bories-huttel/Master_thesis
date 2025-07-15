new_data <- df %>%
  filter(!is.na(cred_d)) %>%
  group_by(cred_d) %>%
  do(data.frame(e_prior = seq(min(.$e_prior), max(.$e_prior), length.out = 100)))

# Predict from the model
preds <- fitted(eq8_2t, newdata = new_data, re_formula = NA, probs = c(0.025, 0.975))
new_data <- bind_cols(new_data, as.data.frame(preds)) 



int_pred <- function(df, model, int_var) {
  new_data <- df |> 
    filter(!is.na({{ int_var }})) |> 
    group_by({{ int_var }}) |> 
    do(data.frame(e_prior = seq(min(.$e_prior), max(.$e_prior), length.out = 100)))
  
  preds <- fitted(model, newdata = new_data, re_formula = NA, probs = c(0.025, 0.975))
  new_data <- bind_cols(new_data, as.data.frame(preds)) 
  
  return(new_data)
}

cred_pred <- int_pred(df, eq8_1, cred_d) 

df_cred <- df |> 
  filter(!is.na(cred_d))

eq8.gg1 <- df_cred |> 
  ggplot(aes(x = e_prior, y = update, color = as.factor(cred_d), group = cred_d)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") + 
  # Raw data points
  geom_point(alpha = 0.3, size = .5, color = "#E6CBCB") +
  #geom_smooth(method = "lm", se = F) +
  
  # Model prediction lines
  geom_line(data = cred_pred, 
            aes(x = e_prior, y = Estimate, color = as.factor(cred_d)), 
            size = 1.2, alpha = .8, lwd = .8) +
  
  stat_summary_bin(data = df_cred ,
                   aes(x = e_prior, y = update, color = as.factor(cred_d), group = cred_d),
                   fun = mean, bins = 20, geom = "point", size = 1) +  
  
  
  # Credible intervals
  geom_ribbon(data = cred_pred, 
              aes(x = e_prior, ymin = Q2.5, ymax = Q97.5, fill = as.factor(cred_d)), 
              alpha = 0.2, color = NA, inherit.aes = FALSE) +
  scale_color_manual(values = c("#B06767", "#911C1C"),
                     labels = c("Low", "High")) +
  scale_fill_manual(values = c("#B06767", "#911C1C"), 
                    labels = c("Low", "High")) +
  
  labs(
       x = "Signal - Prior",
       y = expression(paste(Delta ~"Belief")),
       color = " ") +
  ylim(-60, 60) +
  xlim(-60, 60) +
  guides(fill = "none", 
         x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE), 
         color  = guide_legend(position = "inside")) + 
  theme(legend.position.inside = c(0.2, 0.85), 
        legend.key.size = unit(0.1, 'cm')) + 
  facet_wrap(~"Credibility") 


# 

treat_pred <- int_pred(df, eq8_2, treat) 

df_treat <- df |> 
  filter(!is.na(treat)) 

eq8.gg2 <- df_treat |> 
  ggplot(aes(x = e_prior, y = update, color = as.factor(treat), group = treat)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") + 
  # Raw data points
  geom_point(alpha = 0.3, size = .5, color = "#E6CBCB") +
  #geom_smooth(method = "lm", se = F) +
  
  # Model prediction lines
  geom_line(data = treat_pred, 
            aes(x = e_prior, y = Estimate, color = as.factor(treat)), 
            size = 1.2, alpha = .8, lwd = .8) +
  
  stat_summary_bin(data = df_treat,
                   aes(x = e_prior, y = update, color = as.factor(treat), group = treat),
                   fun = mean, bins = 20, geom = "point", size = 1) +  
  
  
  # Credible intervals
  geom_ribbon(data = treat_pred, 
              aes(x = e_prior, ymin = Q2.5, ymax = Q97.5, fill = as.factor(treat)), 
              alpha = 0.2, color = NA, inherit.aes = FALSE) +
  scale_color_manual(values = c("#B06767", "#911C1C"),
                     labels = c("T1", "T2")) +
  scale_fill_manual(values = c("#B06767", "#911C1C"), 
                    labels = c("T1", "T2")) +
  
  labs(
    x = "Signal - Prior",
    y = expression(paste(Delta ~"Belief")),
    color = "") +
  ylim(-60, 60) +
  xlim(-60, 60) +
  guides(fill = "none", 
         x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE), 
         color  = guide_legend(position = "inside")) + 
  theme(legend.position.inside = c(0.2, 0.85), 
        legend.key.size = unit(0.1, 'cm')) + 
  scale_y_continuous(position = "right") +
  facet_wrap(~"Treatment") 

# 

tenure_pred <- int_pred(df, eq8_3, tenure_d) 

df_tenure <- df |> 
  filter(!is.na(tenure_d)) 

eq8.gg3 <- df_tenure |> 
  ggplot(aes(x = e_prior, y = update, color = as.factor(tenure_d), group = tenure_d)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") + 
  # Raw data points
  geom_point(alpha = 0.3, size = .5, color = "#E6CBCB") +
  #geom_smooth(method = "lm", se = F) +
  
  # Model prediction lines
  geom_line(data = tenure_pred, 
            aes(x = e_prior, y = Estimate, color = as.factor(tenure_d)), 
            size = 1.2, alpha = .8, lwd = .8) +
  
  stat_summary_bin(data = df_tenure,
                   aes(x = e_prior, y = update, color = as.factor(tenure_d), group = tenure_d),
                   fun = mean, bins = 20, geom = "point", size = 1) +  
  
  
  # Credible intervals
  geom_ribbon(data = tenure_pred, 
              aes(x = e_prior, ymin = Q2.5, ymax = Q97.5, fill = as.factor(tenure_d)), 
              alpha = 0.2, color = NA, inherit.aes = FALSE) +
  scale_color_manual(values = c("#B06767", "#911C1C"),
                     labels = c("Renter", "Owner")) +
  scale_fill_manual(values = c("#B06767", "#911C1C"), 
                    labels = c("Renter", "Owner")) +
  
  labs(
    x = "Signal - Prior",
    y = expression(paste(Delta ~"Belief")),
    color = "") +
  ylim(-60, 60) +
  xlim(-60, 60) +
  guides(fill = "none", 
         x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE), 
         color  = guide_legend(position = "inside")) + 
  theme(legend.position.inside = c(0.2, 0.85), 
        legend.key.size = unit(0.1, 'cm')) + 
  facet_wrap(~"Tenure") 

# 
lr_pred <- int_pred(df, eq8_4, lr_scale_d) 

df_lr <- df |> 
  filter(!is.na(lr_scale_d)) 

eq8.gg2 <- df_lr |> 
  ggplot(aes(x = e_prior, y = update, color = as.factor(lr_scale_d), group = lr_scale_d)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") + 
  # Raw data points
  geom_point(alpha = 0.3, size = .5, color = "#E6CBCB") +
  #geom_smooth(method = "lm", se = F) +
  
  # Model prediction lines
  geom_line(data = lr_pred, 
            aes(x = e_prior, y = Estimate, color = as.factor(lr_scale_d)), 
            size = 1.2, alpha = .8, lwd = .8) +
  
  stat_summary_bin(data = df_lr,
                   aes(x = e_prior, y = update, color = as.factor(lr_scale_d), group = lr_scale_d),
                   fun = mean, bins = 20, geom = "point", size = 1) +  
  
  
  # Credible intervals
  geom_ribbon(data = lr_pred, 
              aes(x = e_prior, ymin = Q2.5, ymax = Q97.5, fill = as.factor(lr_scale_d)), 
              alpha = 0.2, color = NA, inherit.aes = FALSE) +
  scale_color_manual(values = c("#B06767", "#911C1C"),
                     labels = c("Left", "Right")) +
  scale_fill_manual(values = c("#B06767", "#911C1C"), 
                    labels = c("Left", "Right")) +
  
  labs(
    x = "Signal - Prior",
    y = expression(paste(Delta ~"Belief")),
    color = "") +
  ylim(-60, 60) +
  xlim(-60, 60) +
  guides(fill = "none", 
         x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE), 
         color  = guide_legend(position = "inside")) + 
  theme(legend.position.inside = c(0.2, 0.85), 
        legend.key.size = unit(0.1, 'cm')) + 
  scale_y_continuous(position = "right") +
  facet_wrap(~"Ideology") 





plot_eq <- function(df1, df2, int_var, lower, upper) {
  df1 %>%
    ggplot(aes(x = e_prior, y = update, color = as.factor({{ int_var }}), group = {{ int_var }})) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "black") + 
    
    # Raw data points
    geom_point(alpha = 0.3, size = .5, color = "#E6CBCB") +
    
    # Model prediction lines
    geom_line(data = df2, 
              aes(x = e_prior, y = Estimate, color = as.factor({{ int_var }})), 
              size = 1.2, alpha = .8) +
    
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


eq8.gg1 <- plot_eq(df_cred, cred_pred, cred_d, lower = "Low", upper = "High") +
  facet_wrap(~"Credibility") + 
  # remove x axis ticks and labels 
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank()) + 
  labs(x = " ", 
       y = expression(paste(Delta ~"Belief")))

eq8.gg2 <- plot_eq(df_treat, treat_pred, treat, lower = "T1", upper = "T2") +
  facet_wrap(~"Treatment") +
  scale_y_continuous(position = "right", limits = c(-60,60)) +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank()) + 
  labs(x = " ", 
       y = "")

eq8.gg3 <- plot_eq(df_tenure, tenure_pred, tenure_d, lower = "Renter", upper = "Owner") +
  facet_wrap(~"Tenure") 


eq8.gg4 <- plot_eq(df_lr, lr_pred, lr_scale_d, lower = "Left", upper = "Right" ) +
  facet_wrap(~"Ideology") + 
  scale_y_continuous(position = "right", limit = c(-60, 60)) + 
  labs(x = "Signal - Prior",  
       y = "")


# Combine plots 
wrap_plots(eq8.gg1, eq8.gg2, eq8.gg3, eq8.gg4, ncol = 2) + 
  plot_layout(axis_titles = "collect")



