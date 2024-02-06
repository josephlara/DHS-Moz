
# PURPOSE:  Munge and Analysis of DHS Results
# AUTHOR:  Joe Lara | USAID
# DATE: 2022-05-12
# NOTES: 

rm(list = ls())

# DEPENDENCIES ------------------------


library(tidyverse)
library(glitr)
library(glamr)
library(googlesheets4)
library(readxl)
library(openxlsx)
library(janitor)
library(glue)
library(scales)
library(ggthemes)
library(fs)
library(patchwork)
library(viridis)
load_secrets()


# LOAD AND PREPARE DATASET ------------------------------------------------


df <- readRDS("Dataout/dhs_kir.rds")

indicators <- distinct(df, area, indicator)
areas <- distinct(df, area)

df2 <- df %>% 
  filter(country != "Mozambique" & group == "Total" | country == "Mozambique") %>% 
  mutate(
    characteristic = case_when(group == "Total" ~ "Total",
                               .default = characteristic),
    group = case_match(group,
                       c("Age 5-year groups", "Age 10-year groups", "Teenager's age") ~ "Age",
                       "Age in months" ~ "Age (months)",
                       "Age (grouped)" ~ "Age (other groups)",
                       "Child's age" ~ "Age (child's)",
                       "Mother's age at birth" ~ "Age (mother's at birth",
                       "Children ever born" ~ "Children ever born (number)",
                       c("Number of living children 6+", "Number of living children grouped") ~ "Living children (nummber)",
                       .default = group))

df_moz <- df2 %>% 
  filter(country == "Mozambique")

df_moz_2022 <- df_moz %>% 
  filter(year == 2022,
         group == "Provinces")

df_moz_geo <- df_moz %>% 
  filter(group == "Provinces")

rm(df, df2, df_moz)


# province definition ----------------------------------------------------------------


focus_province <-  c("Nampula")

cols <- c("Niassa" = "grey70",
          "Cabo Delgado" = "grey70",
          "Nampula" = "#ba0c2f",
          "Zambézia" = "grey70",
          "Tete" = "grey70",
          "Manica" = "grey70",
          "Sofala" = "grey70",
          "Inhambane" = "grey70",
          "Gaza" = "grey70",
          "Maputo Provincia" = "grey70",
          "Maputo Cidade" = "grey70")


# FP ------------------------------------------------------

focus_indicator <- "Children with ARI for whom advice or treatment was sought"

p1 <- df_moz_2022 %>% 
  filter(indicator == focus_indicator) %>% 
  ggplot(aes(x = fct_reorder(characteristic, value), y = value, fill = "grey50")) +
  geom_col(aes(fill = factor(characteristic)), width = 0.85) +
  scale_y_continuous(labels = percent) + 
  theme_fivethirtyeight() +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 12, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 6, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = percent(value, 1)), 
            hjust = 1.5, 
            colour = "white",
            size = 3.5) +
  scale_fill_manual(values = cols) +
  labs(x = NULL,
       y = NULL,
       title = "Children with fever for whom advice or treatment was sought")

p1

# Adolescent Preg ---------------------------------------------------------

focus_indicator <- "Has been pregnant"

p2 <- df_moz_2022 %>% 
  filter(indicator == focus_indicator) %>% 
  ggplot(aes(x = fct_reorder(characteristic, value, .desc = TRUE), y = value, fill = "grey50")) +
  geom_col(aes(fill = factor(characteristic)), width = 0.85) +
  scale_y_continuous(labels = percent) + 
  theme_fivethirtyeight() +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 12, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 6, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = percent(value, 1)), 
            hjust = 1.5, 
            colour = "white",
            size = 3.5) +
  scale_fill_manual(values = cols) +
  labs(x = NULL,
       y = NULL,
       title = "Adolescents who have been pregnant",
       subtitle = "Percentage of women 15-19 years old who report ever having been pregnant")


# 4 ANC ------------------------------------------------------

focus_indicator <- "Antenatal visits for pregnancy: 4+ visits"

p3 <- df_moz_2022 %>% 
  filter(indicator == focus_indicator) %>%   
  ggplot(aes(x = fct_reorder(characteristic, value), y = value, fill = "grey50")) +
  geom_col(aes(fill = factor(characteristic)), width = 0.85) +
  scale_y_continuous(labels = percent) + 
  theme_fivethirtyeight() +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 12, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 6, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = percent(value, 1)), 
            hjust = 1.5, 
            colour = "white",
            size = 3.5) +
  scale_fill_manual(values = cols) +
  labs(x = NULL,
       y = NULL,
       title = "4+ ANC visits",
       subtitle = "Percentage of women who had a live birth in the two years preceding the survey who had 4+ ANC visits")


# CPP in 2 days ------------------------------------------------------


focus_indicator <- "Newborn's first postnatal checkup in the first two days after birth"

p4 <- df_moz_2022 %>% 
  filter(indicator == focus_indicator) %>% 
  ggplot(aes(x = fct_reorder(characteristic, value), y = value, fill = "grey50")) +
  geom_col(aes(fill = factor(characteristic)), width = 0.85) +
  scale_y_continuous(labels = percent) + 
  theme_fivethirtyeight() +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 12, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 6, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = percent(value, 1)), 
            hjust = 1.5, 
            colour = "white",
            size = 3.5) +
  scale_fill_manual(values = cols) +
  labs(x = NULL,
       y = NULL,
       title = "Postnatal checkup within 2 days",
       subtitle = "Percent of newborn's first postnatal checkup in the first two days after birth")

p4

# Malaria Prevalence ------------------------------------------------------


focus_indicator <- "Malaria"

p5 <- df_moz_2022 %>% 
  filter(indicator == focus_indicator) %>%   
  ggplot(aes(x = fct_reorder(characteristic, value, .desc = TRUE), y = value, fill = "grey50")) +
  geom_col(aes(fill = factor(characteristic)), width = 0.85) +
  scale_y_continuous(labels = percent) + 
  theme_fivethirtyeight() +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 12, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 6, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = percent(value, 1)), 
            hjust = 1.5, 
            colour = "white",
            size = 3.5) +
  scale_fill_manual(values = cols) +
  labs(x = NULL,
       y = NULL,
       title = "Malaria prevalence according to RDT",
       subtitle = "Percentage of children age 6-59 months tested using a RDT who are positive for malaria")


# Stunting ------------------------------------------------------


focus_indicator <- "Children stunted"

p6 <- df_moz_2022 %>% 
  filter(indicator == focus_indicator) %>%   
  ggplot(aes(x = fct_reorder(characteristic, value, .desc = TRUE), y = value, fill = "grey50")) +
  geom_col(aes(fill = factor(characteristic)), width = 0.85) +
  scale_y_continuous(labels = percent) + 
  theme_fivethirtyeight() +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 12, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 6, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = percent(value, 1)), 
            hjust = 1.5, 
            colour = "white",
            size = 3.5) +
  scale_fill_manual(values = cols) +
  labs(x = NULL,
       y = NULL,
       title = "Child stunting",
       subtitle = "Percentage of children stunged (below -2 SD of height for age according to WHO standard")



# Patchwork Graphic and Save -------------------------------------------------------


patchwork <- (p1 | p2 | p3) / (p4 | p5 | p6)

patchwork + plot_annotation(
  title = glue('Selected DHS 2022 Indicators, {focus_province}'),
  caption = 'Source: DHS 2022 Key Indicator Report'
)



ggsave(filename = glue('Graphics/dhs_core_ind_SMTN.png'), 
       plot = patchwork,
       width = 10, height = 6)

ggsave(filename = glue('Graphics/dhs_core_ind_{focus_province}.png'), 
       plot = patchwork,
       width = 10, height = 6)


# Line Facet Wrap ---------------------------------------------------------


g1 <- df_moz_geo %>%
  filter(indicator %in% c("Married women currently using any method of contraception",
                          "Has been pregnant",
                          "Antenatal visits for pregnancy: 4+ visits",
                          "Newborn's first postnatal checkup in the first two days after birth",
                          "Malaria",
                          "Children stunted"
  ),
  characteristic == focus_province) %>% 
  mutate(
    indicator = case_match(
      indicator,
      "Married women currently using any method of contraception" ~ "mCPR (married & in-union)",
      "Has been pregnant" ~ "Adolescents who have been pregnant",
      "Antenatal visits for pregnancy: 4+ visits" ~ "4+ ANC visits",
      "Newborn's first postnatal checkup in the first two days after birth" ~ "Postnatal Checkup <2 days",
      "Malaria" ~ "Malaria prevalence among children",
      "Children stunted" ~ "Stunting among children"
    )
  ) %>% 
  
  ggplot(aes(year, value, color = indicator)) + 
  geom_line(linewidth = 1, alpha = .75) +
  geom_point() +
  scale_y_continuous(labels = percent) + 
  theme_fivethirtyeight() +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 12, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 6, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  geom_text(aes(label = percent(value, 1)), 
            hjust = 1.5, 
            size = 3.5) +
  scale_fill_manual(values = cols) +
  labs(x = NULL,
       y = NULL,
       title = glue('Trends Analysis of Selected DHS Indicators in {focus_province}')) +
  facet_wrap(~ indicator)

g1


ggsave(filename = glue('Graphics/dhs_core_trend_SMTN.png'), 
       plot = g1,
       width = 10, height = 6)


ggsave(filename = glue('Graphics/dhs_core_trend_{focus_province}.png'), 
       plot = g1,
       width = 10, height = 6)
