library(tidybayes)
library(tidyverse)
library(brms)
library(ggdist)
library(bayesplot)

m1 <- brm(data = df, 
    family = gaussian, 
    y1i_post ~ 0 + y1i_prior + signal,
    prior = c(prior(beta(2, 2), lb = 0, ub = 1, class = b),
              prior(exponential(11.24), class = sigma)), #exp(sd(df$y1i_post)
    
    warmup = 1000,
    iter = 2000, 
    chains = 4,
    cores = 4,
    seed = 6582) 

hypothesis(m1, "y1i_prior + signal = 1")

m2 <- brm(data = df, 
          family = gaussian, 
          y1i_post ~ 0 + y1i_prior + signal + (0 + signal + y1i_prior | credibility),
          prior = c(prior(beta(2, 2), lb = 0, ub = 1, class = b),
                    prior(exponential(11.24), class = sigma),
                    prior(exponential(.2), class = sd),
                    prior(lkj(2), class = cor)), #exp(sd(df$y1i_post)
          
          warmup = 1000,
          iter = 2000, 
          chains = 4,
          cores = 4,
          seed = 6582) 

## convert credibility to dummy <=4 = 0
df <- df |> 
  mutate(cred_d = case_when(
    credibility <= 4 ~ 0,
    credibility > 4 ~ 1
  ))


df |>
  mutate(d1 = signal - y1i_prior, 
         d2 = y1i_post - y1i_prior) |>
  filter(!is.na(cred_d)) |>
  ggplot(aes(x = d1, y = d2, color = as.factor(cred_d))) +
  geom_point(shape = 21) +
  geom_smooth(method = "lm", se = F) + 
  ## binned skatterplot
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey",lwd=.5) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey", lwd=.5) + 
  labs(x = "Signal - Prior", 
       y = "Posterior - Prior",
       color = "Credibility Group") +
  scale_color_manual(values = c("#E6CBCB", "#911C1C"), 
                     labels = c("Low Credibility", "High Credibility")) 


df <- df |>
  mutate(d1 = signal - y1i_prior, 
         d2 = y1i_post - y1i_prior)

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


posterior <- m2 %>%
  spread_draws(b_Intercept, b_d1, r_credibility[credibility, term]) %>%
  filter(term %in% c("Intercept", "d1")) %>%
  mutate(group_value = case_when(
    term == "Intercept" ~ b_Intercept + r_credibility,
    term == "d1" ~ b_d1 + r_credibility
  ))

posterior |> 
  ggplot(aes(x = credibility, y = group_value)) +
  geom_smooth(aes(x = as.numeric(credibility), y = group_value), method = "lm", se = F, color = "#E6CBCB", lwd = .5) +
  geom_line(aes(x = as.numeric(credibility), group = 1), 
            stat = "summary", fun = median,  color = "#911C1C", lwd = .5) + 
  stat_pointinterval(lwd = .7, color = "#911C1C", alpha = .7, size = 3, shape = 21, fill = "white") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey",lwd=.5) +
  #lines through median
  facet_wrap(~ term, scales = "free_y") 
  #distribution of credibility

library(ggExtra)

credibility_dist <- df %>%
  count(credibility) %>%
  mutate(prop = n / sum(n))  # normalize if you want relative height


posterior %>%
  filter(term == "d1") %>%
  ggplot(aes(x = as.numeric(credibility), y = group_value)) +
  # Distribution as background bars
  geom_col(data = credibility_dist, aes(x = as.numeric(credibility), y = prop * max(posterior$group_value, na.rm = TRUE)), 
           fill = "#E6CBCB", width = 0.4, alpha = 0.3, inherit.aes = FALSE) +
  
  # Main effects
  geom_smooth(method = "lm", se = FALSE, color = "#911C1C", lwd = .5) +
  geom_line(aes(group = 1), stat = "summary", fun = median, color = "#911C1C", lwd = .5) + 
  stat_pointinterval(lwd = .7, color = "#911C1C", alpha = .7, size = 3, shape = 21, fill = "white") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey", lwd = .5) +
  labs(y = "Effect Estimate", x = "Credibility Level") + 
  scale_y_continuous(expand = c(0.003, 0), limits = c(0, 1.1)) +
  xlim