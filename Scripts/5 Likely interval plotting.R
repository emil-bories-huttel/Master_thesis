load("data/cleaned_data.RData") 

library(tidybayes)
library(tidyverse)
library(brms)
library(ggdist)
library(bayesplot)

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

############################
# Load models
############################

# likely intervals 
m_y1_raw <- readRDS("Models/m_y1_raw.rds")

m_y2_raw <- readRDS("Models/m_y2_raw.rds")

m_y3_raw <- readRDS("Models/m_y3_raw.rds")

m_y1_weighted <- readRDS("Models/m_y1_weighted.rds")

m_y2_weighted <- readRDS("Models/m_y2_weighted.rds")

m_y3_weighted <- readRDS("Models/m_y3_weighted.rds")


###############################################
## plot likely range
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

y1_raw_df <- process_model(m_y1_raw, "Most likely", "Unweighted")
y1_weighted_df <- process_model(m_y1_weighted, "Most likely", "Weighted")
y2_raw_df <- process_model(m_y2_raw, "Lower bound", "Unweighted")
y2_weighted_df <- process_model(m_y2_weighted, "Lower bound", "Weighted")
y3_raw_df <- process_model(m_y3_raw, "Upper Bound", "Unweighted")
y3_weighted_df <- process_model(m_y3_weighted, "Upper Bound", "Weighted")

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
  labs(x = "Expected price change", y = "", fill = "") +
  guides(x = guide_axis(minor.ticks = TRUE), 
         fill  = guide_legend(position = "inside")) + 
  theme(panel.grid.major.y = element_line(color = "grey50", linetype = "dotted", size = 0.5), 
        legend.position.inside = c(0.2, 0.85), 
        legend.key.size = unit(0.1, 'cm'))

##########################################
# Individual-level likely ranges
##########################################

df_seg <- df %>%
  mutate(
    distance = abs(y3i_prior - y2i_prior)
  ) %>%
  arrange(distance) %>%
  mutate(
    id = factor(row_number())  # NEW variable for sorting
  )

ggplot(df_seg, aes(x = id)) +
  geom_segment(aes(y = y2i_prior, yend = y3i_prior, xend = id), linewidth = .2, color = "#911C1C") +
  labs(y = "Value", x = "Respondent (sorted by range size)", 
       title = "") +
  theme(
    axis.text.x = element_blank(),  # Hides x-axis text
    axis.ticks.x = element_blank(), 
    panel.grid.major.y = element_line(color = "grey", size = 0.2)) + 
  ylim(-60,60) 
