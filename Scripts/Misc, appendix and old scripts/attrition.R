library(readxl)
library(tidyverse)
library(patchwork)

df <- read_excel("data/raw_uncompleted_respondents.xlsx")

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

##########################
# Clean and wrangle data #
##########################

## Create treatment indicator (1 if t1=0 and 2 if t2=1)

df <- df |> 
  mutate(treat = case_when(
    t1 == 1 ~ 0,
    t2 == 1 ~ 1,
    TRUE ~ NA_real_
  )) 

# position treat left of t1 column in df
df <- df |>  
  relocate(treat, .before = t1)

df <- df |> 
  mutate(
    proximity_prior = coalesce(dev80m_prior, dev500m_prior, dev1k_prior, dev3k_prior, dev5k_prior),
    proximity_post  = coalesce(dev80m_post,  dev500m_post,  dev1k_post,  dev3k_post,  dev5k_post)
  ) |>  
  relocate(proximity_prior, .before = dev80m_prior) |> 
  relocate(proximity_post, .before = dev80m_post)

df <- df |> 
  select(
    -c(dev80m_prior, dev500m_prior, dev1k_prior, dev3k_prior, dev5k_prior,
      dev80m_post, dev500m_post, dev1k_post, dev3k_post, dev5k_post, t1, t2)) 

df_t1 <- df |> 
  filter(treat == 0) 

df_t2 <- df |> 
  filter(treat == 1)

## sum number of non NA per column (all columns)
df_att <- df %>%
  summarise(across(everything(), ~sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "non_na_count") %>%
  mutate(column_number = row_number()) %>%
  select(column_number, non_na_count)


library(tidyverse)

# Step 1: Count non-NA values in each column
df_att <- df %>%
  summarise(across(everything(), ~sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "non_na_count")

# Step 2: Identify and count invalid NAs based on the rule (NA values followed by non-NA to the right)
df_att <- df_att %>%
  mutate(invalid_na_count = mapply(function(col_name) {
    # Get the numeric index of the column based on the column name
    col_index <- which(names(df) == col_name)
    
    # Count how many NAs are invalid based on the rule (NA but not followed by all NAs to the right)
    invalid_na <- sum(apply(df, 1, function(row) {
      # If the current value is NA, check if any values to the right are non-NA
      if (is.na(row[col_index])) {
        any(!is.na(row[(col_index + 1):ncol(df)]))  # Check if any value to the right is non-NA
      } else {
        FALSE
      }
    }))
    
    # Return the count of invalid NAs
    invalid_na
  }, names(df)))

# Step 3: Add the column number and select the final result
df_att <- df_att %>%
  mutate(column_number = row_number()) %>%
  select(column_number, non_na_count, invalid_na_count) |> 
  mutate(sum = non_na_count + invalid_na_count) 

# Step 1: Count non-NA values in each column
df_att1 <- df_t1 %>%
  summarise(across(everything(), ~sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "non_na_count")

# Step 2: Identify and count invalid NAs based on the rule (NA values followed by non-NA to the right)
df_att1 <- df_att1 %>%
  mutate(invalid_na_count = mapply(function(col_name) {
    # Get the numeric index of the column based on the column name
    col_index <- which(names(df) == col_name)
    
    # Count how many NAs are invalid based on the rule (NA but not followed by all NAs to the right)
    invalid_na <- sum(apply(df, 1, function(row) {
      # If the current value is NA, check if any values to the right are non-NA
      if (is.na(row[col_index])) {
        any(!is.na(row[(col_index + 1):ncol(df)]))  # Check if any value to the right is non-NA
      } else {
        FALSE
      }
    }))
    
    # Return the count of invalid NAs
    invalid_na
  }, names(df)))

# Step 3: Add the column number and select the final result
df_att1 <- df_att1 %>%
  mutate(column_number = row_number()) %>%
  select(column_number, non_na_count, invalid_na_count) |> 
  mutate(sum = non_na_count + invalid_na_count)

# Step 1: Count non-NA values in each column
df_att2 <- df_t2 %>%
  summarise(across(everything(), ~sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "non_na_count")

# Step 2: Identify and count invalid NAs based on the rule (NA values followed by non-NA to the right)
df_att2 <- df_att2 %>%
  mutate(invalid_na_count = mapply(function(col_name) {
    # Get the numeric index of the column based on the column name
    col_index <- which(names(df) == col_name)
    
    # Count how many NAs are invalid based on the rule (NA but not followed by all NAs to the right)
    invalid_na <- sum(apply(df, 1, function(row) {
      # If the current value is NA, check if any values to the right are non-NA
      if (is.na(row[col_index])) {
        any(!is.na(row[(col_index + 1):ncol(df)]))  # Check if any value to the right is non-NA
      } else {
        FALSE
      }
    }))
    
    # Return the count of invalid NAs
    invalid_na
  }, names(df)))

# Step 3: Add the column number and select the final result
df_att2 <- df_att2 %>%
  mutate(column_number = row_number()) %>%
  select(column_number, non_na_count, invalid_na_count) |> 
  mutate(sum = non_na_count + invalid_na_count)

# df_att columnnumber > 17 = NA 
df_att <- df_att |> 
  mutate(sum1 = ifelse(column_number > 17, NA, sum))

df_att1 <- df_att1 |> 
  mutate(sum1 = ifelse(column_number < 17, NA, sum))

df_att2 <- df_att2 |> 
  mutate(sum1 = ifelse(column_number < 17, NA, sum))

gg1 <- ggplot(df_att, aes(x = column_number, y = sum)) +
  geom_line(color = "#911C1C") +
  labs(
    x     = "Question Index",
    y     = "Participants Remaining"
  ) + 
  geom_vline(xintercept = c(10,17), linetype = "dashed", color = "black") + 
  xlim(0, 32) + 
  ylim(400, 1500) +
  facet_wrap(~"General attrition")

gg2 <- ggplot(df_att1, aes(x = column_number, y = sum)) +
  geom_line(color = "#911C1C") +
  geom_line(data = df_att2, aes(x = column_number, y = sum1), color = "#B06767") +
  labs(
    x     = "Question Index",
    y     = "",
  ) + 
  xlim(19,32) +
  scale_y_continuous(position = "right") + 
  facet_wrap(~"Post-treatment attrition by group")

wrap_plots(gg1, gg2, ncol = 2) + 
  plot_layout(axis_titles = "collect_x") 
