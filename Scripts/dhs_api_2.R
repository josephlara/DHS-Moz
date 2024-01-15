
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
library(sf)
load_secrets()


# DHS INDICATOR SELECTION --------------------------------------------------------


tags <- dhs_tags() |> arrange(TagID)
indicators <- dhs_indicators()

# indicators[1,]
# tags[grepl("Stunting", indicators$Definition), ]
# tags[grepl("Malaria", tags$TagName), ]
# 
# indicator_focus <- indicators %>% 
#   select(TagIds, Label, IndicatorId) %>% 
#   filter(TagIds == 0)


# data <- dhs_data(indicatorIds = "CN_NUTS_C_HA2", countryIds = "MZ", breakdown = "subnational", surveyYearStart = 1997)
# 
# data <- dhs_data(tagIds = 36,countryIds = c("CD","TZ"),breakdown="subnational",surveyYearStart = 2010)


# FUNCTIONS ---------------------------------------------------------------

plot_nat_mult_trend <- function(indicators, type = "Percent", title, subtitle) {
  
  df <- dhs_data(indicatorIds = indicators,
                 countryIds = c("MZ"),
                 surveyYearStart = 1997) %>% 
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
  
  
  p <- df %>% 
    ggplot(aes(survey_year, value, color = indicator)) +
    geom_line(linewidth = 1, alpha = .75) +
    si_style_ygrid() +
    theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),  # previously #e7e7e5
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
         title = title,
         subtitle = subtitle,
         caption = "Source: https://www.statcompiler.com/")
  
  return(p)
  
}
plot_nat_trend <- function(indicator, color_hex, type = "Percent", c_interval = TRUE) {
  
  # prepare df for plot and define whether the values should be percent or absolute
  # valid type values are "Percent" and "Absolute"
  df <- dhs_data(indicatorIds = indicator,
                 countryIds = c("MZ"),
                 surveyYearStart = 1997) |> 
    filter(IsPreferred == 1) |> # in testing phase
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
    theme(plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
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
  
  
  if (c_interval == TRUE) {
    
    p <- p +
      
      geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = .1)
    
  } else if (c_interval == FALSE) {
    
    p <- p
    
  }
  
  p <- p +
    labs(x = "",
         y = "",
         title = str_wrap(indicator_label, width = 50),
         subtitle = str_wrap(indicator_definition, width = 130),
         caption = "Source: https://www.statcompiler.com/")

  return(p)

}
plot_nat_disag <- function(indicator, survey_year, breakdown, palette = "rocket", reverse_ord = FALSE, type = "Percent") {
  
  # prepare df for plot and define whether the values should be percent or absolute
  # valid type values are "Percent" and "Absolute"
  
  df <- dhs_data(indicatorIds = indicator,
                 countryIds = c("MZ"),
                 surveyYear	= survey_year,
                 breakdown = "all") |> 
    filter(IsPreferred == 1) |> # in testing phase
    select(CountryName, SurveyType, SurveyYear, Indicator, CharacteristicCategory, CharacteristicLabel, Value, CIHigh, CILow) |> 
    filter(CharacteristicCategory == breakdown)
  
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
  
  
  # fetch plot label and subtitle text values from "indicators" object
  indicator_label <- filter(indicators, IndicatorId == indicator)$Label
  indicator_definition <- filter(indicators, IndicatorId == indicator)$Definition
  
  # plot indicator
  p <- df |> 
    ggplot(aes(x = fct_reorder(characteristic_label, value, .desc = reverse_ord), y = value, fill = value)) +
    scale_fill_viridis(option = palette, direction = -1, end = .8) +
    geom_col(alpha = .75) +
    si_style_xgrid() +
    coord_flip() +
    theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
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
                hjust = -.25,
                vjust = .5) +
      scale_y_continuous(labels = percent,
                         expand = expansion(mult = 0.1)) 
    
  } else if (type == "Absolute") {
    
    p <- p +
      
      geom_text(aes(label = value),
                size = 3,
                hjust = -.25,
                vjust = .5) +
      scale_y_continuous(expand = expansion(mult = 0.1)) 
    
  }
  
  p <- p +
    labs(x = "",
         y = "",
         title = str_wrap(indicator_label, width = 50),
         subtitle = str_wrap(indicator_definition, width = 130),
         caption = "Source: https://www.statcompiler.com/")
  
  return(p)
  
}

# BREADOWN CHOICES
# [1] "Total"                       "Sex"                         "Age in months"              
# [4] "Residence"                   "Education"                   "Education (2 groups)"       
# [7] "Wealth quintile"             "Mother's interview status"   "Size at birth"              
# [10] "Birth interval"              "Mother's nutritional status" "Region"   

# PALETTE CHOICES
# "magma" (or "A")
# "inferno" (or "B")
# "plasma" (or "C")
# "viridis" (or "D")
# "cividis" (or "E")
# "rocket" (or "F")
# "mako" (or "G")
# "turbo" (or "H")

# EARLY CHILDHOOD MORTALITY VISUALIZATION -------------------------------------------------


indicator_list <- c("CM_ECMT_C_U5M", "CM_ECMT_C_NNR", "CM_ECMT_C_IMR")


# plot of neonatal, infant, and child mortality
plot_nat_mult_trend(indicators = indicator_list,
                    type = "Absolute",
                    title = "Early Child Mortality",
                    subtitle = "Neonatal, Infant, and Child Mortality (5 year periods)")


# plots of individual early childhood mortality indicators
plot_nat_trend(indicator = "CM_ECMT_C_NNR",
               color_hex = "#00C19A",
               type = "Absolute",
               c_interval = TRUE)


plot_nat_trend(indicator = "CM_ECMT_C_IMR",
               color_hex = "#8494FF",
               type = "Absolute",
               c_interval = TRUE)


plot_nat_trend(indicator = "CM_ECMT_C_CMR",
               color_hex = "#ABA300",
               type = "Absolute",
               c_interval = TRUE)


# plot of stunting
plot_nat_trend(indicator = "FP_CUSM_W_MOD",
               color_hex = "#E68613",
               c_interval = FALSE) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1))


# plot of stunting by breakdown option
plot_nat_disag(indicator = "FP_CUSM_W_ANY", 
               survey_year = 2011, 
               breakdown = "Region", 
               palette = "plasma",
               reverse_ord = FALSE, 
               type = "Percent")





# MAPPING -----------------------------------------------------------------





df_geo <- dhs_geometry(countryIds = "MZ",
                     surveyYear = 2011)


df_geo_shp <- st_as_sf(df_geo,
                       coords = c("Coordinates"))


|> 
  select(RegionID, Coordinates)

df <- dhs_data(indicatorIds = "CM_ECMT_C_CMR",
               countryIds = c("MZ"),
               surveyYear	= 2011,
               breakdown = "subnational") |> 
  filter(IsPreferred == 1) |> # in testing phase
  select(CountryName, SurveyType, SurveyYear, Indicator, CharacteristicCategory, CharacteristicLabel, RegionId, Value, CIHigh, CILow)
  # filter(CharacteristicCategory == "all")


df_map <- df |> 
  left_join(
    df_geo,
    by = join_by(RegionId == RegionID)
  )


ggplot(data = df_map,
       mapping = aes(fill = Value))



# HELP IN GETTING IndicatorIDs --------------------------------------------

df <- dhs_data(tagIds = 32)

unique(df$Indicator)

# plot_nat_trend
df <- dhs_data(indicatorIds = "CH_VACC_C_NON",
               countryIds = c("MZ"),
               surveyYearStart = 1997) |> 
  filter(IsPreferred == 1) |>
  select(CountryName, SurveyType, SurveyYear, Indicator, Value, CIHigh, CILow)

df
unique(df$Indicator)

