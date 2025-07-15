rm(list=ls())

library(tidyverse) 
library(readxl) 
library(lubridate)
library(patchwork)

custom_theme <- theme_bw() + 
  theme(
    text = element_text(family = "Times"),    
    panel.grid = element_blank(), 
    panel.border = element_rect(colour = "black", fill = NA),
    axis.ticks.length = unit(.15, "cm"), 
    strip.background = element_blank()
  ) 

theme_set(custom_theme)

###########
# PRICES #
##########

## IMPORT DATA 
real_estate <- read_excel("Housing market stats/Samlede priser.xlsx")

real_estate$`Transaction price realised` <- as.numeric(real_estate$`Transaction price realised`) 


## Convert quarters to dates (e.g. 1992Q2 --> 1992-06-01)
real_estate <- real_estate %>%
  mutate(time = case_when(
    grepl("Q1", Time) ~ ymd(paste0(substr(Time, 1, 4), "-01-01")),
    grepl("Q2", Time) ~ ymd(paste0(substr(Time, 1, 4), "-04-01")),
    grepl("Q3", Time) ~ ymd(paste0(substr(Time, 1, 4), "-07-01")),
    grepl("Q4", Time) ~ ymd(paste0(substr(Time, 1, 4), "-10-01"))
  ))




## plot  
gg.prices <- real_estate |>
  ggplot(aes(x = time, y = `Transaction price realised`/1000)) + 
  geom_line(aes(group = Municipality), alpha = 0.3, color = "#E6CBCB") +
  geom_line(data = real_estate |> 
              filter(Municipality %in% c("København", "Århus")), 
            aes(x = time, y = `Transaction price realised`/1000, color = Municipality)) +
  geom_point(data = real_estate |> 
               filter(Municipality %in% c("København", "Århus")), 
            aes(x = time, y = `Transaction price realised`/1000, color = Municipality), size = .5) +
  scale_x_date(
    date_breaks = "2 years",     
    date_labels = "%Y", 
    expand = c(0.03, 0.05)) +
  ylim(0, 100) +
  labs(x = "",
        y = "m² price (thousand DKK)", 
        color = "") + 
  facet_wrap(~Type) + 
  scale_color_manual(values = c("København" = "#911C1C", "Århus" = "#B06767")) +
  guides(x = guide_axis(minor.ticks = TRUE),
           y = guide_axis(minor.ticks = TRUE), 
           color  = guide_legend(position = "inside")) + 
  theme(legend.position.inside = c(0.1, 0.85), 
        legend.key.size = unit(0.1, 'cm'),  
        axis.text.x = element_text(angle = 45, hjust = 1)) 

###########
# Supply #
########## 

## IMPORT DATA 
supply <- read_excel("Housing market stats/Supply.xlsx")

supply$`Available dwellings` <- as.numeric(supply$`Available dwellings`)

## Convert quarters to dates (e.g. 1992M1 --> 1992-01-01)

supply <- supply %>%
  mutate(time = case_when(
    grepl("M01", Time) ~ ymd(paste0(substr(Time, 1, 4), "-01-01")),  # For M1, January
    grepl("M02", Time) ~ ymd(paste0(substr(Time, 1, 4), "-02-01")),  # For M2, February
    grepl("M03", Time) ~ ymd(paste0(substr(Time, 1, 4), "-03-01")),  # For M3, March
    grepl("M04", Time) ~ ymd(paste0(substr(Time, 1, 4), "-04-01")),  # For M4, April
    grepl("M05", Time) ~ ymd(paste0(substr(Time, 1, 4), "-05-01")),  # For M5, May
    grepl("M06", Time) ~ ymd(paste0(substr(Time, 1, 4), "-06-01")),  # For M6, June
    grepl("M07", Time) ~ ymd(paste0(substr(Time, 1, 4), "-07-01")),  # For M7, July
    grepl("M08", Time) ~ ymd(paste0(substr(Time, 1, 4), "-08-01")),  # For M8, August
    grepl("M09", Time) ~ ymd(paste0(substr(Time, 1, 4), "-09-01")),  # For M9, September
    grepl("M10", Time) ~ ymd(paste0(substr(Time, 1, 4), "-10-01")),  # For M10, October
    grepl("M11", Time) ~ ymd(paste0(substr(Time, 1, 4), "-11-01")),  # For M11, November
    grepl("M12", Time) ~ ymd(paste0(substr(Time, 1, 4), "-12-01"))   # For M12, December
  ))

## plot
gg.sup <- supply |>
  ggplot(aes(x = time, y = `Available dwellings`/100)) + 
  geom_line(aes(group = Municipality), alpha = 0.3, color = "#E6CBCB") +
  #geom_smooth(data = supply |> filter(Municipality %in% c("København", "Århus")), 
              #aes(x = time, y = `Available dwellings`/100, color = Municipality), 
              #method = "lm", linetype = "dotted", se = FALSE, lwd = .4) +
  geom_line(data = supply 
            |>  filter(Municipality %in% c("København", "Århus")), 
            aes(x = time, y = `Available dwellings`/100, color = Municipality)) +
  geom_point(data = supply |> 
               filter(Municipality %in% c("København", "Århus")), 
             aes(x = time, y = `Available dwellings`/100, color = Municipality), size = .5) + 
  scale_x_date(
    date_breaks = "2 years",         
    date_labels = "%Y", 
    expand = c(0.03, 0.05)) +
  ylim(0, 40) +
  labs(x = "",
       y = "Availabe dwellings (hundreds)", 
       color = "") + 
  facet_wrap(~Type) + 
  scale_color_manual(values = c("København" = "#911C1C", "Århus" = "#B06767")) +
  guides(x = guide_axis(minor.ticks = TRUE),
         y = guide_axis(minor.ticks = TRUE), 
         color  = guide_legend(position = "inside")) + 
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.text = element_blank()) 


## wrap plots 
wrap_plots(gg.prices, gg.sup, ncol = 1) 


###########
# RENT #
##########

## IMPORT DATA
rent <- read_excel("Housing market stats/Husleje.xlsx")

rent <- rent %>%
  arrange(municipality, year) %>%
  group_by(municipality) %>%
  mutate(
    absolute_change = rent - lag(rent),
    percent_change = (rent - lag(rent)) / lag(rent) * 100
  )

## year as date 
rent$year <- as.Date(paste0(rent$year, "-01-01"))

print(rent)

## plot 
abs <- rent |> 
  ggplot(aes(x = year, y = rent)) + 
  geom_line(aes(group = municipality), alpha = 0.3, color = "#E6CBCB") +
  geom_line(data = rent |> filter(municipality %in% c("København", "Aarhus")), 
            aes(x = year, y = rent, color = municipality)) +
  geom_point(data = rent |> filter(municipality %in% c("København", "Aarhus")), 
             aes(x = year, y = rent, color = municipality), size = .5) +
  labs(x = "",
       y = "m² price (DKK)", 
       color = "") +
  scale_x_date(
    date_breaks = "1 years",         
    date_labels = "%Y", 
    expand = c(0.03, 0.05)) +
  ylim(500, 1500) +
  scale_color_manual(values = c("København" = "#911C1C", "Aarhus" = "#B06767")) +
  guides(x = guide_axis(minor.ticks = FALSE),
         y = guide_axis(minor.ticks = TRUE), 
         color  = guide_legend(position = "inside")) + 
  theme(legend.position.inside = c(0.20, 0.85), 
        legend.key.size = unit(0.1, 'cm'),  
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~"Absolute rent trends")

rent_pct <- rent |> 
  filter(!is.na(percent_change)) 

yoy <- rent_pct |>
  ggplot(aes(x = year, y = percent_change)) + 
  geom_line(aes(group = municipality), alpha = 0.3, color = "#E6CBCB") +
  geom_line(data = rent_pct |> filter(municipality %in% c("København", "Aarhus")), 
            aes(x = year, y = percent_change, color = municipality)) +
  geom_point(data = rent_pct |> filter(municipality %in% c("København", "Aarhus")), 
             aes(x = year, y = percent_change, color = municipality), size = .5) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  labs(x = "",
       y = "Yearly growth rate (%)", 
       color = "") +
  scale_x_date(
    date_breaks = "1 years",         
    date_labels = "%Y", 
    expand = c(0.03, 0.05)) +
  scale_color_manual(values = c("København" = "#911C1C", "Aarhus" = "#B06767")) +
  guides(x = guide_axis(minor.ticks = FALSE),
         y = guide_axis(minor.ticks = TRUE), 
         color  = guide_legend(position = "inside")) + 
  theme(legend.position = "none", 
        legend.key.size = unit(0.1, 'cm'),  
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~"Yearly rent growth trends") + 
  scale_y_continuous(position = "right", limits = c(-2,12)) 

wrap_plots(abs, yoy, ncol = 2)





mean(rent$percent_change, na.rm = TRUE)
    
rent |> 
  group_by(municipality) |>
  summarise(mean_percent_change = mean(percent_change, na.rm = TRUE)) |> 
  ## max
  filter(mean_percent_change == min(mean_percent_change)) 


# Cumulative change (for survey) 

rent_cum <- rent %>% 
  arrange(municipality, year) %>%
  group_by(municipality) %>%
  mutate(
    base_rent = first(rent[year(year) == 2015]),  # Extract base rent for 2015
    cumulative_change = (rent / base_rent) * 100
  ) %>%
  ungroup()


rent_cum |>
  ggplot(aes(x = year, y = cumulative_change)) + 
  geom_line(aes(group = municipality), alpha = 0.3, color = "#679db8") +
  geom_line(data = rent_cum |> filter(municipality %in% c("København", "Aarhus")), 
            aes(x = year, y = cumulative_change, color = municipality)) +
  geom_point(data = rent_cum |> filter(municipality %in% c("København", "Aarhus")), 
             aes(x = year, y = cumulative_change, color = municipality), size = .5) + 
  geom_hline(yintercept = 100, linetype = "dashed", color = "black") + 
  labs(x = "",
       y = "Indeks (2015 = 100)", 
       color = "", 
       caption = "Kilde: Social- og Boligstyrelsen") +
  scale_x_date(
    date_breaks = "1 years",         
    date_labels = "%Y", 
    expand = c(0.03, 0.05)) +
  scale_color_manual(values = c("København" = "#003d73", "Aarhus" = "#0484c2")) +
  guides(x = guide_axis(minor.ticks = FALSE),
         y = guide_axis(minor.ticks = TRUE), 
         color  = guide_legend(position = "inside")) + 
  theme(legend.position.inside = c(0.2, 0.85), 
        legend.key.size = unit(0.1, 'cm'),  
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~"Procentvis stigning i husleje siden 2015 (alle kommuner)") 
  ## add footnote
  
  


ggsave("Housing market stats/Husleje.png", width = 8, height = 5, dpi = 500)

## save with larger font
ggsave("Housing market stats/Husleje.png", width = 8, height = 5, dpi = 500, pointsize = 20)

rent_cum |>
  filter(municipality == "København") |>
  select(year, cumulative_change, rent) 

rent_pct |> 
  group_by(municipality) |> 
  # sort by average yearly growth 
  summarise(mean_percent_change = mean(percent_change, na.rm = TRUE)) |>
  arrange(mean_percent_change) |>
  print(n = 98)

mean(rent_pct$percent_change, na.rm = TRUE)
