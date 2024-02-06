
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

indicator_list <- c("CM_ECMT_C_U5M", "CM_ECMT_C_NNR", "CM_ECMT_C_IMR")
breakdown <- "Region"


# FUNCTIONS -----------------------------------------------------------------------

#break into 2 functions: 1 to grab df and 2 to either plot or table

#grab df function
grab_nat_disagg <- function(indicator, survey_year, breakdown) {
  
  df <- dhs_data(indicatorIds = indicator,
                 countryIds = c("MZ"),
                 surveyYear	= survey_year,
                 breakdown = "all") |> 
    filter(IsPreferred == 1) |> # in testing phase
    select(CountryName, SurveyType, SurveyYear, Indicator, CharacteristicCategory, CharacteristicLabel, CharacteristicId, Value, CIHigh, CILow) |> 
    filter(CharacteristicCategory == breakdown)
  
  
  return(df)
  
}

#plot function
plot_nat_disag <- function(df, indicator, survey_year, breakdown, palette = "rocket", reverse_ord = FALSE, type = "Percent") {
  
  # prepare df for plot and define whether the values should be percent or absolute
  # valid type values are "Percent" and "Absolute"
  
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

#gt table function - fix type param
tbl_nat_disagg <- function(df, indicator, survey_year, breakdown, type) {

  # fetch plot label and subtitle text values from "indicators" object
  indicator_label <- filter(indicators, IndicatorId == indicator)$Label
  indicator_definition <- filter(indicators, IndicatorId == indicator)$Definition
  
  df %>% 
    mutate(Value = Value / 100) %>%
    relocate(SurveyType, .after = "CILow") %>%
    relocate(SurveyYear, .before = "Value") %>% 
    arrange(desc(Value)) %>% 
    gt() %>% 
    fmt_percent(columns = Value, 
                decimals = 0) %>%   
    cols_hide(columns = c("CountryName", "Indicator", "CharacteristicCategory", "CharacteristicId")) %>% 
    cols_label(CharacteristicLabel = "Region", #change this to breakdown
               SurveyYear = "Year", # to survey_year
               SurveyType = "Survey Type", 
               Value = "Value",
               CILow = "Lower Bound",
               CIHigh = "Upper Bound") %>% 
    cols_align(align = "left", columns = 1) %>% 
    tab_style(
      style = list(
        cell_borders(
          sides = c("left"),
          color = trolley_grey_light,
          weight = px(2)
        )
      ),
      locations = list(
        cells_body(
          columns = c(6, 9)
        )
      )
    ) %>%
    tab_header(
      title = glue("{indicator_label}"),
      subtitle = glue("{indicator_definition}")) %>% 
    tab_source_note(
      source_note = gt::md(glue("Source: {df$Source %>% unique()}"))) %>% 
    tab_options(
      source_notes.font.size = px(10)) %>% 
    gt_theme_nytimes() 
  
}

# TEST ----------------------------------------------------------------------------------

#plot - should include year in plot
grab_nat_disagg(indicator = "FP_CUSM_W_ANY", survey_year = 2011, breakdown = "Region") %>% 
  plot_nat_disag(indicator = "FP_CUSM_W_ANY", survey_year = 2011, breakdown = "Region",
                 palette = "rocket", reverse_ord = FALSE, type = "Percent")



#GT table -
  #add year into title rather than a column
  #add NA for CIs
  # source note at bottom of table and plot
df <- grab_nat_disagg(indicator = "FP_CUSM_W_ANY", survey_year = 2011, breakdown = "Region") 

grab_nat_disagg(indicator = "FP_CUSM_W_ANY", survey_year = 2011, breakdown = "Region") %>% 
  tbl_nat_disagg(indicator = "FP_CUSM_W_ANY", survey_year = 2011, breakdown = "Region", type = "Percent")



# GIS MAPPING ---------------------------------------------------------------

#ask Joe about levels for mapping params
  # is region the lowest level we'll look to
  # map only works with breakdown = Region; return error to user

map_nat_disagg <- function(df, cntry)

cntry <- "Mozambique"

spdf <- gisr::get_vcpolygons()

df_orgs <- grabr::datim_orgunits(cntry = cntry, reshape = TRUE)

spdf_cntry <- df_orgs %>% 
  filter(level == 3) %>% 
  left_join(spdf, ., by = c("uid" = "orgunituid")) %>% 
  filter(!is.na(orgunit))


spdf_snu <- df_orgs %>% 
  filter(level == 4) %>% 
  left_join(spdf, ., by = c("uid" = "orgunituid")) %>% 
  filter(!is.na(orgunit))

df_nat <- grab_nat_disagg(indicator = "FP_CUSM_W_ANY", survey_year = 2011, breakdown = "Region") %>% 
  mutate(CharacteristicLabel  = case_when(CharacteristicLabel  == "Zambézia" ~ "Zambezia",
                                          CharacteristicLabel  == "Maputo Cidade" ~ "Cidade De Maputo",
                                          CharacteristicLabel  == "Maputo Provincia" ~ "Maputo",
                                            TRUE ~ CharacteristicLabel ))

spdf_snu_map <- spdf_snu %>% 
  left_join(df_nat, by = c("orgunit" = "CharacteristicLabel")) %>% 
  mutate(Value = Value / 100)


# VIZ -------------------------------------------------------------------


spdf_snu_map %>%
  # filter(funding_agency == "USAID") %>% 
  ggplot() +
  geom_sf(data = spdf_cntry, aes(geometry = geometry), fill = "white") +
  #geom_sf(data = adm0_mwi, aes(geometry = geometry), fill = grey10k) +
  geom_sf(data = spdf_snu_map, aes(fill = Value), alpha = 0.8) +
  scale_fill_si(palette = "scooters") +
  ggplot2::geom_sf_text(data = spdf_snu_map,
                        ggplot2::aes(label = orgunit),
                        family = "Source Sans Pro") +
  ggplot2::geom_sf_text(data = spdf_snu_map,
                        nudge_y =-1,
                        ggplot2::aes(label = scales::percent(Value)),
                        family = "Source Sans Pro") +
  labs(x = NULL,
       y = NULL,
       title = glue("{indicator_label}") %>% toupper(),
       subtitle = glue("{indicator_definition}")) +
  si_style_map()


