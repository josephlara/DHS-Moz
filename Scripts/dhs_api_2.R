
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


# FUNCTIONS ---------------------------------------------------------------


plot_nat_trend <- function(indicator, color_hex, type = "Percent") {
  
  # prepare df for plot and define whether the values should be percent or absolute
  # valid type values are "Percent" and "Absolute"

  df <- dhs_data(indicatorIds = indicator,
                 countryIds = c("MZ"),
                 surveyYearStart = 1997) |> 
    select(CountryName, SurveyType, SurveyYear, Indicator, Value, CIHigh, CILow) 
    
    if (type == "Percent") {
      
      df <- df |> 
      
      mutate(Value = ifelse(is.na(Value), NA, Value / 100),
             CIHigh = ifelse(CIHigh == "", NA, CIHigh / 100),
             CILow = ifelse(CILow == "", NA, CILow / 100))
        
    } else if (type == "Absolute") {
      
      df <- df |> 
      
      mutate(Value = ifelse(is.na(Value), NA, Value),
             CIHigh = ifelse(CIHigh == "", NA, CIHigh),
             CILow = ifelse(CILow == "", NA, CILow))
      
    }
    
  df <- clean_names(df)
  
  
  # obtain plot label and subtitle text values from "indicators" object
  indicator_label <- filter(indicators, IndicatorId == indicator)$Label
  indicator_definition <- filter(indicators, IndicatorId == indicator)$Definition

  # plot indicator
  p <- df |> 
    ggplot(aes(survey_year, value)) +
    geom_line(linewidth = 1, alpha = .75, color = color_hex) +
    si_style_ygrid() +
    theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
          plot.title = element_text(size = 16, vjust = 3),
          plot.subtitle = element_text(size = 8, vjust = 6, color = "grey40"),
          plot.caption = element_text(face = "italic", size = 9, vjust = 1),
          panel.spacing = unit(.75, "cm"),
          axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust=1),
          legend.position = "none",
          legend.direction = "vertical",
          legend.title = element_blank())
  
  
  if (type == "Percent") {
    
    p <- p +
      
      geom_text(aes(label = scales::percent(value, 1)),
                size = 3,
                vjust = -.75,
                hjust = .5) +
      scale_x_continuous(breaks = c(1980:2022),
                         expand = expansion(mult = 0.1)) + # ensures geom_text data labels are not cut off
      scale_y_continuous(labels = percent,
                         expand = expansion(mult = 0.1)) 
    
  } else if (type == "Absolute") {
    
    p <- p +
      
      geom_text(aes(label = value),
                size = 3,
                vjust = -.75,
                hjust = .5) +
      scale_x_continuous(breaks = c(1980:2022),
                         expand = expansion(mult = 0.1)) + # ensures geom_text data labels are not cut off
      scale_y_continuous(expand = expansion(mult = 0.1)) 
    
  }
  
  p <- p +
    labs(x = "",
         y = "",
         title = indicator_label,
         subtitle = str_wrap(indicator_definition, width = 115),
         caption = "Source: https://www.statcompiler.com/")

  return(p)

}

plot_nat_mult_trend <- function(indicators, title, subtitle) {
  
  df <- dhs_data(indicatorIds = indicators,
                 countryIds = c("MZ"),
                 surveyYearStart = 1997) %>% 
    select(CountryName, SurveyType, SurveyYear, Indicator, Value, CIHigh, CILow) %>% 
    mutate(Value = Value / 100) %>% 
    clean_names()
  
  p <- df %>% 
    ggplot(aes(survey_year, value, color = indicator)) +
    geom_line(size = 1, alpha = .75) +
    si_style_ygrid() +
    theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
          plot.title = element_text(size = 16, vjust = 4),
          plot.subtitle = element_text(size = 8, vjust = 6, color = "grey50"),
          plot.caption = element_text(face = "italic", size = 9, vjust = 1),
          panel.spacing = unit(.75, "cm"),
          axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust=1),
          legend.position = "none",
          legend.direction = "vertical",
          legend.title = element_blank()) +
    geom_text(aes(label = value),
              size = 3.5,
              vjust = -.75, 
              hjust = .5) +
    scale_x_continuous(breaks = c(1980:2022),
                       expand = expansion(mult = 0.1)) + # ensures geom_text data labels are not cut off
    scale_y_continuous(expand = expansion(mult = 0.1)) + # ensures geom_text data labels are not cut off
    labs(x = "",
         y = "",
         title = title_plot,
         subtitle = subtitle_plot,
         caption = "Source: https://www.statcompiler.com/")
  
  return(p)
  
}


# DHS INDICATOR SELECTION --------------------------------------------------------


tags <- dhs_tags()
indicators <- dhs_indicators()

indicator_focus <- indicators %>% 
  select(TagIds, Label, IndicatorId) %>% 
  filter(TagIds == 14)


# EARLY CHILDHOOD MORTALITY VISUALIZATION -------------------------------------------------


indicator_list <- c("CM_ECMT_C_U5M", "CM_ECMT_C_NNR", "CM_ECMT_C_IMR")
title_plot <- "Early Childhood Mortality"
subtitle_plot <- "Neonatal, Infant, and Child Mortality (5 year periods)"

# plot of neonatal, infant, and child mortality
plot_nat_mult_trend(indicators = indicator_list,
                    title = "Early Child Mortality")

# plots of individual early childhood mortality indicators
plot_nat_trend(indicator = "CM_ECMT_C_NNR",
               color_hex = "#00C19A",
               type = "Absolute") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = .1)

plot_nat_trend(indicator = "CM_ECMT_C_IMR",
               color_hex = "#8494FF") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = .1)

plot_nat_trend(indicator = "CM_ECMT_C_CMR",
               color_hex = "#ABA300") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = .1)


# plot of 
plot_nat_trend(indicator = "CN_NUTS_C_HA2",
               color_hex = "#E68613") +
  scale_y_continuous(labels = percent,
                     limits = c(0, .5))


