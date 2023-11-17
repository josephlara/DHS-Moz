
# PURPOSE:  Munge and Analysis of DHS Results
# AUTHOR:  Joe Lara | USAID
# DATE: 2022-05-12
# NOTES: 

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
library(viridis)
library(rdhs)
load_secrets()


# DHS INDICATOR SELECTION --------------------------------------------------------


tags <- dhs_tags()
indicators <- dhs_indicators()

indicator_focus <- indicators %>% 
  select(TagIds, Label, IndicatorId) %>% 
  filter(TagIds == 75)


# EARLY CHILDHOOD MORTALITY VISUALIZATION -------------------------------------------------


indicator <- c("CM_ECMT_C_U5M", "CM_ECMT_C_NNR", "CM_ECMT_C_IMR")


df_child_mortality <- dhs_data(indicatorIds = indicator,
                 countryIds = c("MZ"),
                 surveyYearStart = 1997) %>% 
  select(CountryName, SurveyType, SurveyYear, Indicator, Value, CIHigh, CILow) %>% 
  clean_names()


df_child_mortality %>% 
  ggplot(aes(survey_year, value, color = indicator)) +
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(face = "italic", size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = value),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks = c(1980:2022)) +
  labs(x = "",
       y = "",
       title = "Under-five Mortality Rate (1997-2022)",
       caption = "Source: https://www.statcompiler.com/")


# UNDER-FIVE MORTALITY ----------------------------------------------------


indicator <- "CM_ECMT_C_U5M"

data <- dhs_data(indicatorIds = indicator,
                 countryIds = c("MZ"),
                 surveyYearStart = 1997) %>% 
  select(CountryName, SurveyType, SurveyYear, Indicator, Value, CIHigh, CILow) %>% 
  clean_names()


indicator_label <- filter(indicators, IndicatorId == indicator)$Label
indicator_definition <- filter(indicators, IndicatorId == indicator)$Definition


ggplot(data, aes(survey_year, value)) +
  geom_line(size = 1, alpha = .75) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = .1) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(face = "italic", size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = value),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks = c(1980:2022)) +
  labs(x = "",
       y = "",
       title = indicator_label,
       subtitle = indicator_definition,
       caption = "Source: https://www.statcompiler.com/")





