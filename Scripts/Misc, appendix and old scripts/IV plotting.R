
new_data <- df |> 
  do(data.frame(update = seq(min(.$update), max(.$update), length.out = 100)))

eq9_1_df <- posterior_linpred(eq9_1) |> 
  as.data.frame() 

df %>%
  data_grid(update = seq_range(update, n = 100)) %>%
  add_epred_draws(eq10_1, resp = "updatedevd") %>%
  ggplot(aes(x = update, y = .epred, fill = after_stat(.width))) +
  stat_lineribbon(.width = ppoints(50), lwd = 1) +
  scale_fill_distiller() +
  labs(x = "Update", y = "Predicted Update_dev_d",
       title = "Second Stage Effect: Posterior Predictive Regression Line") +
  theme_minimal()

df %>%
  data_grid(update = seq_range(update, n = 100)) %>%
  add_epred_draws(eq10_2, resp = "updatedevd") %>%
  ggplot(aes(x = update, y = .epred, fill_ramp = after_stat(.width))) +
  stat_lineribbon(.width = ppoints(50), lwd = .5, fill = "#911C1C", color = "#911C1C") +
  scale_fill_ramp_continuous(range = c(1, 0)) +
  labs(x = "Update", y = "Predicted Update_dev_d",
       title = "Second Stage Effect: Posterior Predictive Regression Line") 

df %>%
  data_grid(update = seq_range(update, n = 100)) %>%
  add_epred_draws(eq10_2, resp = "updatedevd", ndraws = 100) %>%
  ggplot(aes(x = update, y = .epred, group = .draw)) +
  geom_line(alpha = 0.2, color = "") +
  labs(
    x = "Update", 
    y = "Predicted Update_dev_d",
    title = "Second Stage Effect: 100 Posterior Predictive Lines"
  ) +
  theme_minimal()

library(modelr)

  
extract_coef_eq10 <- function(model, var1, var2, type, eq) {
  model %>%
    spread_draws({{ var1 }}, {{ var2 }}) |>
    mutate(name = type, 
           equation = eq) |> 
    rename(second_stage = {{ var1 }},
           first_stage = {{ var2 }})
}

eq10_1_draws <- extract_coef_eq10(eq10_1, 
                                 b_updatedevd_update, b_update_treat, 
                                 "Centered prior", "Change")
eq10_2_draws <- extract_coef_eq10(eq10_2, 
                                 b_updatedevd_update, b_update_treat, 
                                 "Directional prior", "Change")
eq10_3_draws <- extract_coef_eq10(eq10_3, 
                                  b_prodevpostd_y1i_post, b_y1ipost_treat, 
                                  "Centered prior", "Levels")
eq10_4_draws <- extract_coef_eq10(eq10_4, 
                                  b_prodevpostd_y1i_post, b_y1ipost_treat, 
                                  "Directional prior", "Levels")

df_draws <- bind_rows(eq10_1_draws, eq10_2_draws, eq10_3_draws, eq10_4_draws) 

df_draws |> 
  ggplot(aes(x = second_stage, fill = name)) +
  geom_density(alpha = 0.8, color= NA) +
  labs(x = "Coefficient", y = "Density") +
  scale_fill_manual(values = c("#911C1C", "#B06767")) +
  theme(legend.position = "top") + 
  facet_wrap(~ equation)


df_summary <- df_draws %>%
  group_by(equation, name) %>%
  summarise(
    s_q16.7 = quantile(second_stage, 0.167),
    s_q83.3 = quantile(second_stage, 0.833),
    s_q2.5 = quantile(second_stage, 0.025),
    s_q97.5 = quantile(second_stage, 0.975),
    s_median = median(second_stage),
    f_q16.7 = quantile(first_stage, 0.167),
    f_q83.3 = quantile(first_stage, 0.833),
    f_q2.5 = quantile(first_stage, 0.025),
    f_q97.5 = quantile(first_stage, 0.975),
    f_median = median(first_stage),
    .groups = "drop"
  )

eq10.gg1 <- df_summary |> 
  ggplot(aes(x = equation, y = f_median, fill = name, color = name)) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey40") +
  geom_boxplot(aes(ymax=f_q97.5, ymin=f_q2.5, lower = f_q16.7, upper = f_q83.3, middle = f_median),
               stat = "identity",  alpha = 0.8, width = 0.2, 
               position = position_dodge(width = 0.3) 
               ) + 
  labs(x = "", 
       y = "First stage", 
       color = "", 
       fill = "") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0), 
        axis.ticks.y = element_blank(), 
        legend.key.size = unit(0.15, 'cm'), 
        legend.position.inside = c(0.75, 0.20)) + 
  coord_flip() + 
  geom_vline(xintercept = 1.5, color = "black") + 
  scale_color_manual(values = c("#911C1C", "#B06767")) +
  scale_fill_manual(values = c("#911C1C", "#B06767")) +
  scale_y_continuous(position = "right", limits = c(-6, 6)) + 
  guides(x = guide_axis(minor.ticks = TRUE), 
         color  = guide_legend(position = "inside")) 

eq10.gg2 <- df_summary |> 
  ggplot(aes(x = equation, y = s_median, 
             fill = name, color = name)) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey40") +
  geom_boxplot(aes(ymax=s_q97.5, ymin=s_q2.5, lower = s_q16.7, upper = s_q83.3, middle = s_median),
                            stat = "identity",  alpha = 0.8, width = 0.2, 
                            position = position_dodge(width = 0.3))+
  labs(x = "", 
       y = "Second stage", 
       color = "", 
       fill = "") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0), 
        axis.ticks.y = element_blank(), 
        legend.position = "none") + 
  ylim(-0.05, 0.05) +
  coord_flip() + 
  geom_vline(xintercept = 1.5, color = "black") + 
  scale_color_manual(values = c("#911C1C", "#B06767")) +
  scale_fill_manual(values = c("#911C1C", "#B06767")) +
  guides(x = guide_axis(minor.ticks = TRUE))

wrap_plots(eq10.gg1, eq10.gg2, ncol = 1)

