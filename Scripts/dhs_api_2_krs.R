
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
    cols_hide(columns = c("CountryName", "Indicator", "CharacteristicCategory")) %>% 
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



#GT table
df <- grab_nat_disagg(indicator = "FP_CUSM_W_ANY", survey_year = 2011, breakdown = "Region") 

grab_nat_disagg(indicator = "FP_CUSM_W_ANY", survey_year = 2011, breakdown = "Region") %>% 
  tbl_nat_disagg(indicator = "FP_CUSM_W_ANY", survey_year = 2011, breakdown = "Region", type = "Percent")



# GIS MAPPING ---------------------------------------------------------------


# shape files ----------------------------------------------------------

library(gagglr)

shpdata <- glamr::si_path("path_vector")


# Load the shapefiles to grab boundaries from below
spdf_pepfar <- get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")
cntry <- "Mozambique"

adm0 <- gisr::get_admin0(cntry)
adm1 <- gisr::get_admin1(cntry)

#get country level
cntry_lvl = 3
spdf_cntry <- spdf_pepfar %>% 
  extract_boundaries(country = cntry, 
                     level = cntry_lvl)

#get snu level
snu_lvl = 4

spdf_snu <- spdf_pepfar %>% 
  extract_boundaries(country = cntry, 
                     level = snu_lvl) %>% 
  left_join(df_incidence, by = c("orgunit" = "area"))






df_geo <- dhs_geometry(countryIds = "MZ",
                       surveyYear = 2011)


df_geo_shp <- st_as_sf(df_geo,
                       coords = c("Coordinates")) |> 
  select(RegionID, Coordinates)

df_new <- dhs_data(indicatorIds = "FP_CUSM_W_ANY",
                   countryIds = c("MZ"),
                   surveyYear	= 2011,
                   breakdown = "Region") |> 
  filter(IsPreferred == 1) |> # in testing phase
  select(CountryName, SurveyType, SurveyYear, Indicator, CharacteristicCategory, CharacteristicLabel, RegionId, Value, CIHigh, CILow)
# filter(CharacteristicCategory == "all")

df_new <- grab_nat_disagg(indicator = "FP_CUSM_W_ANY", survey_year = 2011, breakdown = "Region") %>% 
  mutate(CharacteristicId = as.character(CharacteristicId))

df_map <- df_new |> 
  left_join(
    df_geo,
    by = join_by(CharacteristicId == RegionID)
  )


ggplot(data = df_map,
       mapping = aes(fill = Value))

