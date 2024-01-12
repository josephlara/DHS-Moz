
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
    # value = ifelse(!indicator %in% c("Total fertility rate 15-49",
    #                                       "Neonatal mortality rate 5 year periods",
    #                                       "Infant mortality rate 5 year periods",
    #                                       "Under-five mortality rate 5 year periods",
    #                                       "Mean height for age of children",
    #                                       "Mean weight for height of children",
    #                                       "Mean weight for age of children",
    #                                       "Mean number of sexual partners in lifetime [Women]",
    #                                       "Mean number of sexual partners in lifetime [Men]"),
    #                     value * 100,
    #                     value),
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

unique(df_moz_2022$indicator)

rm(df, df2, df_moz)


# Color coding ----------------------------------------------------------------

cols <- c("Niassa" = "grey70",
          "Cabo Delgado" = "grey70",
          "Nampula" = "#3e5daf",
          "Zambézia" = "grey70",
          "Tete" = "grey70",
          "Manica" = "grey70",
          "Sofala" = "grey70",
          "Inhambane" = "grey70",
          "Gaza" = "grey70",
          "Maputo Provincia" = "grey70",
          "Maputo Cidade" = "grey70")

focus_province <-  "Nampula"

# Adolescent Preg ---------------------------------------------------------

focus_indicator <- "Has been pregnant"

df <- df_moz_2022 %>% 
  filter(
    indicator == focus_indicator)


p <- df %>%  
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


ggsave(filename = "Graphics/adolescent_preg.png", plot = p, width = 4, height = 4)


df <- df_moz_geo %>% 
  filter(
    indicator == focus_indicator,
    characteristic == focus_province)

p <- df %>%  
  ggplot(aes(year, value, color = indicator)) + 
  geom_line(linewidth = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=c(2011, 2022), labels=c('DHS 2011', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Prevalence of Anemia in Women by Province",
       subtitle = " * Percentage of women 15-49 classified as having any anemia (<12.0 g/dl for non-pregnant women and <11.0 g/dl for pregnant women)\n * Percentage of women 15-49 classified as having mild anemia (11.0-11.9 g/dl for non-pregnant women and 10.0-10.9 g/dl for pregnant women)\n * Percentage of women 15-49 classified as having moderate anemia (8.0-10.9 g/dl for non-pregnant women and 7.0-9.9 g/dl for pregnant women)\n * Percentage of women 15-49 classified as having severe anemia (<8.0 g/dl for non-pregnant women and <7.0 g/dl for pregnant women)",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 34")


# Malaria Prevalence ------------------------------------------------------


df <- df_moz_2022 %>% 
  filter(
    indicator == "Malaria")


p <- df %>%  
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


ggsave(filename = "Graphics/malaria_prev.png", plot = p, width = 4, height = 4)


# FP ------------------------------------------------------


df <- df_moz_2022 %>% 
  filter(
    indicator == "Married women currently using any method of contraception")


p <- df %>%  
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
       title = "Current use of anticontraceptive methods",
       subtitle = "Percentage distribution among woment currently married or in union and among sexually active women age")
       

ggsave(filename = "Graphics/fp_prev.png", plot = p, width = 4, height = 4)



# 4 ANC ------------------------------------------------------


df <- df_moz_2022 %>% 
  filter(
    indicator == "Antenatal visits for pregnancy: 4+ visits")


p <- df %>%  
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


ggsave(filename = "Graphics/anc_4visits.png", plot = p, width = 4, height = 4)


# CPP in 2 days ------------------------------------------------------


df <- df_moz_2022 %>% 
  filter(
    indicator == "Newborn's first postnatal checkup in the first two days after birth")


p <- df %>%  
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


ggsave(filename = "Graphics/cpp_2days.png", plot = p, width = 4, height = 3)


# Wasting ------------------------------------------------------


df <- df_moz_2022 %>% 
  filter(
    indicator == "Children stunted")


p <- df %>%  
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


ggsave(filename = "Graphics/nutr_stunting.png", plot = p, width = 4, height = 4)






# Various Indicators ------------------------------------------------------

df <- df_moz_geo %>% 
  filter(indicator %in% 
           c("Has been pregnant",
             "Malaria",
             "Married women currently using any method of contraception",
             "Antenatal visits for pregnancy: 4+ visits",
             "Newborn's first postnatal checkup in the first two days after birth",
             "Children stunted"
           ),
         characteristic == "Nampula") %>% 
  mutate(indicator =
           case_match(indicator,
                      "Antenatal visits for pregnancy: 4+ visits" ~ "4+ ANC",
                      "Married women currently using any method of contraception" ~ "Using Modern FP",
                      "Newborn's first postnatal checkup in the first two days after birth" ~ "Postnatal checkup <2 days",
                      "Malaria" ~ "Malaria Prevalence",
                      .default = indicator)
  )

unique(df$indicator)

df %>% 
  ggplot(aes(year, value, color = indicator)) + 
  geom_line(size = 1, alpha = .75) +
  geom_point(size = 1.5, alpha = .9) +
  scale_y_continuous(labels = percent) + 
  si_style_ygrid() +
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
            # colour = "white",
            size = 3) +
  scale_fill_manual(values = cols) +
  labs(x = NULL,
       y = NULL,
       title = "Time Trend of Core Survey Indicators (1997 - 2022)") + 
  facet_wrap(~ indicator)

