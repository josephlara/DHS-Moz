# DEPENDENCIES ------------------------


library(tidyverse)
library(janitor)
library(gt)
library(scales)
library(glue)
library(rdhs)
library(googlesheets4)
library(glamr)
library(glitr)
library(viridis)
load_secrets()


indicator_codes <- rdhs::dhs_indicators()

country_codes <- rdhs::dhs_countries() |> 
  dplyr::filter(str_detect(SubregionName, "Africa"),
                !is.na(DHS_CountryCode)) |>
  dplyr::distinct(DHS_CountryCode) |> 
  dplyr::pull()



df <- save_nat_disagg(indicator = "MM_MMRO_W_MMR", country = country_codes, survey_year_start = 1990) |> 
  dplyr::mutate(year_char = as.character(SurveyYear)) |> 
  dplyr::mutate(country_year = paste(CountryName, year_char, sep = ", ")) |> 
  dplyr::mutate(country_flag = case_when(CountryName == "Mozambique" ~ TRUE,
                                         .default = FALSE))

df_indicator <- df |> 
  distinct(Indicator) |> 
  pull()


df |> 
  ggplot(aes(x = fct_reorder(country_year, Value), 
             y = Value,
             fill = country_flag)) +
  coord_flip() +
  geom_col() +
  theme(
    plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
    panel.spacing = unit(.75, "cm"),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(
      size = 14, 
      hjust = 0,
      vjust = 0),
    plot.subtitle = element_text(
      face = "italic", 
      size = 9, 
      vjust = 0, 
      color = "grey30"),
    plot.caption = element_text(
      size = 9,
      color = "grey50"),
    legend.position = "none",
    legend.title = element_blank()
  ) +
  labs(x = "",
       y = "",
       title = df_indicator,
       subtitle = str_wrap("Maternal mortality ratio for the seven years preceding the survey expressed per 100,000 live births", 75),
       caption = "Source: https://www.statcompiler.com/")
