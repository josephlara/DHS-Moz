
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
library(gt)
library(gtExtras)
library(glue)
load_secrets()


#NOTE from KS: these indicators arent working - only able to test with FP_CUSM_W_ANY
indicator_list <- c("CM_ECMT_C_U5M", "CM_ECMT_C_NNR", "CM_ECMT_C_IMR")
breakdown <- "Region"


# GLOBAL FUNCTIONS -----------------------------------------------------------------------

#grab df function
grab_nat_disagg <- function(indicator, survey_year, breakdown) {
  
  df <- rdhs::dhs_data(indicatorIds = indicator,
                       countryIds = c("MZ"),
                       surveyYear	= survey_year,
                       breakdown = "all") |> 
    dplyr::filter(IsPreferred == 1) |> # in testing phase
    dplyr::select(CountryName, SurveyType, SurveyYear, Indicator, CharacteristicCategory, CharacteristicLabel, CharacteristicId, Value, CIHigh, CILow) |> 
    dplyr::filter(CharacteristicCategory == breakdown) %>% 
    dplyr::mutate(CIHigh = ifelse(CIHigh == "", NA, CIHigh),
                  CILow = ifelse(CILow == "", NA, CILow))
  
  
  return(df)
  
}

#plot function
plot_nat_disag <- function(df, indicator, survey_year, breakdown, palette = "rocket", reverse_ord = FALSE, type = "Percent") {
  
  # prepare df for plot and define whether the values should be percent or absolute
  # valid type values are "Percent" and "Absolute"
  
  if (type == "Percent") {
    
    df <- df |>
      
      dplyr::mutate(Value = ifelse(is.na(Value), NA, Value / 100),
                    CIHigh = ifelse(CIHigh == "", NA, CIHigh / 100),
                    CILow = ifelse(CILow == "", NA, CILow / 100))
    
  } else if (type == "Absolute") {
    
    df <- df |>
      
      dplyr::mutate(Value = ifelse(is.na(Value), NA, Value),
                    CIHigh = ifelse(CIHigh == "", NA, CIHigh),
                    CILow = ifelse(CILow == "", NA, CILow))
    
  }
  
  df <- janitor::clean_names(df)
  
  survey_source <- df %>% 
    dplyr::pull(survey_type) %>% 
    unique()
  
  # fetch plot label and subtitle text values from "indicators" object
  indicator_label <-  dplyr::filter(indicators, IndicatorId == indicator)$Label
  indicator_definition <-  dplyr::filter(indicators, IndicatorId == indicator)$Definition
  
  # plot indicator
  p <- df |>
    ggplot2::ggplot(aes(x = fct_reorder(characteristic_label, value, .desc = reverse_ord), y = value, fill = value)) +
    scale_fill_viridis(option = palette, direction = -1, end = .8) +
    ggplot2::geom_col(alpha = .75) +
    glitr::si_style_xgrid() +
    ggplot2::coord_flip() +
    ggplot2::theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
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
      
      ggplot2::geom_text(aes(label = scales::percent(value, 1)),
                         size = 3,
                         hjust = -.25,
                         vjust = .5) +
      ggplot2::scale_y_continuous(labels = percent,
                                  expand = expansion(mult = 0.1))
    
  } else if (type == "Absolute") {
    
    p <- p +
      
      ggplot2::geom_text(aes(label = value),
                         size = 3,
                         hjust = -.25,
                         vjust = .5) +
      ggplot2::scale_y_continuous(expand = expansion(mult = 0.1))
    
  }
  
  p <- p +
    ggplot2::labs(x = "",
                  y = "",
                  title = glue("{indicator_label} in {survey_year}"),
                  subtitle = glue("{indicator_definition}"),
                  caption = glue("Source: {survey_source} {survey_year}
                        https://www.statcompiler.com/"))
  
  return(p)
  
}

#gt table function - fix type param
tbl_nat_disagg <- function(df, indicator, survey_year, breakdown, type) {
  
  # fetch plot label and subtitle text values from "indicators" object
  indicator_label <- dplyr::filter(indicators, IndicatorId == indicator)$Label
  indicator_definition <- dplyr::filter(indicators, IndicatorId == indicator)$Definition
  
  survey_source <- df %>% 
    dplyr::pull(SurveyType) %>% 
    unique()
  
  df %>% 
    dplyr::mutate(Value = Value / 100) %>%
    dplyr::relocate(SurveyType, .after = "CILow") %>%
    dplyr::relocate(SurveyYear, .before = "Value") %>% 
    dplyr::arrange(desc(Value)) %>% 
    gt::gt() %>% 
    gt::fmt_percent(columns = Value, 
                    decimals = 0) %>%   
    gt::sub_missing(missing_text = ".") %>% 
    gt::cols_hide(columns = c("CountryName", "Indicator","SurveyYear", "SurveyType",
                              "CharacteristicCategory", "CharacteristicId")) %>% 
    gt::cols_label(CharacteristicLabel = "Region", #change this to breakdown
                   SurveyYear = "Year", # to survey_year
                   SurveyType = "Survey Type", 
                   Value = "Value",
                   CILow = "Lower Bound",
                   CIHigh = "Upper Bound") %>% 
    gt::cols_align(align = "left", columns = 1) %>% 
    gt::tab_style(
      style = list(
        cell_borders(
          sides = c("left"),
          color = trolley_grey_light,
          weight = px(2)
        )
      ),
      locations = list(
        cells_body(
          columns = c()
        )
      )
    ) %>%
    gt::tab_header(
      title = glue("{indicator_label} in {survey_year}"),
      subtitle = glue("{indicator_definition}")) %>% 
    gt::tab_source_note(
      source_note = gt::md(glue("Source: {survey_source}: {survey_year}
                                 https://www.statcompiler.com/"))) %>% 
    gt::tab_options(
      source_notes.font.size = px(10)) %>% 
    gtExtras::gt_theme_nytimes() 
  
}

# map function

map_nat_disagg <- function(cntry, indicator, survey_year, breakdown = "Region") {
  
  if (breakdown == "Region") {
    
    spdf <- gisr::get_vcpolygons()
    df_orgs <- grabr::datim_orgunits(cntry = cntry, reshape = TRUE)
    
    spdf_cntry <- df_orgs %>% 
      dplyr::filter(level == 3) %>% 
      dplyr::left_join(spdf, ., by = c("uid" = "orgunituid")) %>% 
      dplyr::filter(!is.na(orgunit))
    
    spdf_snu <- df_orgs %>% 
      dplyr::filter(level == 4) %>% 
      dplyr::left_join(spdf, ., by = c("uid" = "orgunituid")) %>% 
      dplyr::filter(!is.na(orgunit))
    
    df_nat <- grab_nat_disagg(indicator = indicator, survey_year = survey_year, breakdown = breakdown) %>% 
      dplyr::mutate(CharacteristicLabel  = dplyr::case_when(CharacteristicLabel  == "Zambézia" ~ "Zambezia",
                                                            CharacteristicLabel  == "Maputo Cidade" ~ "Cidade De Maputo",
                                                            CharacteristicLabel  == "Maputo Provincia" ~ "Maputo",
                                                            TRUE ~ CharacteristicLabel ))
    
    spdf_snu_map <- spdf_snu %>% 
      dplyr::left_join(df_nat, by = c("orgunit" = "CharacteristicLabel")) %>% 
      dplyr::mutate(Value = Value / 100)
    
    spdf_snu_map %>%
      # filter(funding_agency == "USAID") %>% 
      ggplot2::ggplot() +
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
           title = glue("{indicator_label} in {survey_year}") %>% toupper(),
           subtitle = glue("{indicator_definition}"),
           caption = glue("Source: DHS {survey_year}
                           https://www.statcompiler.com/")) +
      si_style_map() +
      theme(legend.position = "none")
    
    
  } else if (breakdown != "Region") {
    print("ERROR: Map only at Region level")
  }
  
}


# WRAPPER FUNCTION ----------------------------------------------------------

dhs_wrapper <- function(indicator, survey_year, breakdown, value_type,output_type) {
  
  if (output_type == "Plot") {

    #plot - should include year in plot
    grab_nat_disagg(indicator = indicator, survey_year = survey_year, breakdown = breakdown) %>% 
      plot_nat_disag(indicator = indicator, survey_year = survey_year, breakdown = breakdown,
                     palette = "rocket", reverse_ord = FALSE, type = value_type)
    
  } else if (output_type == "Table") {
    grab_nat_disagg(indicator = indicator, survey_year = survey_year, breakdown = breakdown) %>% 
      tbl_nat_disagg(indicator = indicator, survey_year = survey_year, breakdown = breakdown,
                     type = value_type)
    
  } else if (output_type == "Map") {
    
    map_nat_disagg(cntry = "Mozambique",
                   indicator = indicator, survey_year = 2011, breakdown = breakdown) 
    
  }
}

# TEST ----------------------------------------------------------------------------------

dhs_wrapper(indicator = "FP_CUSM_W_ANY", survey_year = 2011, breakdown = "Region",
            value_type = "Percent",
            output_type = "Table") # change this to Plot or Table

dhs_wrapper(indicator = "FP_CUSM_W_ANY", survey_year = 2011, breakdown = "District",
            value_type = "Percent",
            output_type = "Map")








