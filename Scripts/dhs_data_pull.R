
# PURPOSE:  DHS API Data Pull
# AUTHOR:  Joe Lara | USAID
# DATE: 2022-05-12
# NOTES: 

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

# DATA DEPENDENCIES ----------------------------------------------------------

indicator_menu <- googlesheets4::read_sheet(ss = "1KMtnnNDtMCvDQAwhjldHAAyx8zvBd1oUcT_sSGzSwu8", 
                                            sheet = "ids") |> 
  dplyr::arrange(Level1) |> 
  dplyr::relocate(IndicatorId, .after = dplyr::everything())


indicator_codes <- rdhs::dhs_indicators()

list_indicator <- indicator_menu |> 
  dplyr::select(IndicatorId) |> 
  dplyr::distinct(IndicatorId) |> 
  dplyr::pull()

country_codes <- rdhs::dhs_countries() |> 
  dplyr::filter(str_detect(SubregionName, "Africa"),
                !is.na(DHS_CountryCode)) |>
  dplyr::distinct(DHS_CountryCode) |> 
  dplyr::pull()


df <- read_csv("Dataout/df.csv")


# LOAD FUNCTIONS ----------------------------------------------------------


# indicators <- rdhs::dhs_indicators()


# 
# df_level <- indicators |>
#   dplyr::select(IndicatorId, Level1) |> 
#   dplyr::distinct()
# 
# 
# df_definition <- indicators |>
#   dplyr::select(IndicatorId, Definition) |> 
#   dplyr::distinct() |> 
#   dplyr::group_by(Definition) |> 
#   dplyr::slice(1)
# 
# 
# test <- df_definition |> 
#   group_by(Definition) |> 
#   slice(1)


# working map function to pull data

df <- map(list_indicator, ~ save_nat_disagg(.x, survey_year_start = 1997)) |> 
  bind_rows()

write_csv(df, 
          "Dataout/df_fertility.csv")


save_nat_disagg <- function(indicator, country = "MZ", survey_year_start) {
  tryCatch({
    df <- rdhs::dhs_data(indicatorIds = indicator,
                         countryIds = country,
                         surveyYearStart = survey_year_start,
                         breakdown = "all")
    
    if (nrow(df) == 0) {
      warning("No records returned for the given query.")
      return(NULL)
    }
    
    df <- df |>
      dplyr::filter(IsPreferred == 1) |> # in testing phase
      dplyr::select(CountryName, DHS_CountryCode,SurveyType, SurveyId, SurveyYear, Indicator, IndicatorId, ByVariableLabel, IsPreferred, CharacteristicCategory, CharacteristicLabel, Value, CIHigh, CILow) |>
      dplyr::mutate(CIHigh = ifelse(CIHigh == "", NA, CIHigh),
                    CILow = ifelse(CILow == "", NA, CILow))
    
    indicators <- rdhs::dhs_indicators()
    
    pubs <- rdhs::dhs_publications() |>
      dplyr::filter(PublicationTitle == "Final Report") |> 
      dplyr::select(SurveyId, PublicationURL) |> 
      dplyr::distinct()
    
    df_level <- indicators |>
      dplyr::select(IndicatorId, Level1) |> 
      dplyr::distinct()
    
    # df_definition <- indicators |>
    #   dplyr::select(IndicatorId, Definition) |> 
    #   dplyr::distinct() |> 
    #   dplyr::group_by(Definition) |> 
    #   dplyr::slice(1)
    
    df <- df |>
      dplyr::left_join(df_level, join_by(IndicatorId == IndicatorId)) |>
      # dplyr::left_join(df_definition, join_by(IndicatorId)) |>
      dplyr::left_join(pubs, join_by(SurveyId == SurveyId)) |>
      dplyr::relocate(Level1, .before = Indicator) |>
      # dplyr::relocate(Definition, .after = Indicator) |>
      dplyr::select(!SurveyId)
    
    # val_area <- indicators |>
    #   dplyr::filter(IndicatorId == indicator) |>
    #   dplyr::distinct(Level1) |>
    #   dplyr::pull()
    # val_indicator <- df |> dplyr::distinct(Indicator) |> pull()
    # val_filename <- glue::glue('Data/{val_area} - {val_indicator}.csv')
    
    return(df)
    
  }, error = function(e) {
    errorMessage <- paste("An error occurred:", conditionMessage(e))
    warning(errorMessage)
    return(NULL)
  })
}


plot_nat_disagg <- function(df, indicator, survey_year = 2022, breakdown = "Region", palette = "rocket", type = "Percent", reverse_ord = FALSE) {
  
  df <- df |> 
    dplyr::filter(IndicatorId == indicator,
                  SurveyYear == survey_year,
                  CharacteristicCategory == breakdown)
  
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
  
  survey_source <- df |> 
    dplyr::pull(survey_type) |> 
    unique()
  
  indicator_label <- df |> 
    dplyr::pull(indicator) |> 
    unique()
  
  publication_url <- df |> 
    dplyr::pull(publication_url) |> 
    unique()
  
  
  p <- df |>
    ggplot2::ggplot(aes(x = fct_reorder(characteristic_label, value, .desc = reverse_ord), y = value, fill = value)) +
    scale_fill_viridis(option = palette, direction = -1, end = .8) +
    ggplot2::geom_col(alpha = .75) +
    ggplot2::geom_col(alpha = .75) +
    glitr::si_style_xgrid() +
    ggplot2::coord_flip() +
    ggplot2::theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
                   plot.title = element_text(size = 14, vjust = 3),
                   plot.subtitle = element_text(size = 8, vjust = 6, color = "grey60"),
                   plot.caption = element_text(color = "grey50", size = 7, vjust = 1),
                   panel.spacing = unit(.75, "cm"),
                   axis.text.x = element_text(size = 9, angle = 0, vjust = 1, hjust = .5),
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
                  title = glue("{indicator_label} ({survey_source} {survey_year})"),
                  # subtitle = glue("{indicator_definition}"),
                  caption = glue("Source: {survey_source} {survey_year}
                                 https://www.statcompiler.com/
                                  {publication_url}"))
  
  return(p)
  
}

tbl_nat_disagg <- function(df, indicator, survey_year = 2022, breakdown = "Region", type = "Percent", reverse_ord = FALSE) {
  
  df <- df |>  
    dplyr::filter(IndicatorId == indicator,
                  SurveyYear == survey_year,
                  CharacteristicCategory == breakdown)
  
  if (reverse_ord == FALSE) {
    
    df <- df |>  
      
      dplyr::arrange(desc(Value))
    
  } else if (reverse_ord == TRUE) {
    
    df <- df |>  
      
      dplyr::arrange(Value)
    
  }
  
  survey_source <- df |> 
    dplyr::pull(SurveyType) |> 
    unique()
  
  survey_year <- df |> 
    dplyr::pull(SurveyYear) |> 
    unique()
  
  indicator_label <- df |> 
    dplyr::pull(Indicator) |> 
    unique()
  
  df |> 
    # dplyr::arrange(desc(Value)) |> 
    dplyr::mutate(Value = Value / 100) |>
    dplyr::relocate(SurveyType, .after = "CILow") |>
    dplyr::relocate(SurveyYear, .before = "Value") |> 
    gt::gt() |> 
    gt::fmt_percent(columns = Value, 
                    decimals = 0) |>   
    gt::sub_missing(missing_text = ".") |> 
    gt::cols_hide(columns = c("CountryName", "DHS_CountryCode", "Level1", "Indicator", "IndicatorId", 
                              "ByVariableLabel", "IsPreferred", "PublicationURL",  "SurveyYear", 
                              "SurveyType", "CharacteristicCategory")) |> 
    gt::cols_label(CharacteristicLabel = breakdown, #change this to breakdown
                   Value = "Value",
                   CILow = "Lower Bound",
                   CIHigh = "Upper Bound") |> 
    gt::cols_align(align = "left", columns = 1) |> 
    gt::tab_style(
      style = list(
        cell_borders(
          sides = c("left"),
          color = trolley_grey_light,
          weight = px(2)
        )
      ),
      locations = list(
        gt::cells_body(
          columns = c()
        )
      )
    ) |>
    gt::tab_header(
      title = glue("{indicator_label} ({survey_year})")) |>
    gt::tab_source_note(
      source_note = gt::md(glue("Source:  {survey_source} {survey_year} \n  
                                 https://www.statcompiler.com/"))) |>
    gt::tab_options(
      source_notes.font.size = px(9)) |> 
    gtExtras::gt_theme_nytimes()
  
}




# TESTING -----------------------------------------------------------------



df <- save_nat_disagg(indicator = "MM_MMRO_W_MMR", country = country_codes, survey_year_start = 1990) |> 
  dplyr::mutate(year_char = as.character(SurveyYear)) |> 
  dplyr::mutate(country_year = paste(CountryName, year_char, sep = ", "))

df |> tbl_nat_disagg(indicator = "CH_VACC_C_OP3", 
                     breakdown = "Region", 
                     survey_year = 2022,
                     reverse_ord = FALSE)


df |> plot_nat_disagg(indicator = "CH_VACC_C_OP3", 
                      survey_year = 2022, 
                      breakdown = "Region", 
                      type = "Percent", 
                      reverse_ord = FALSE)



ref <- df |>  
  dplyr::filter(IndicatorId == "CH_VACC_C_OP3", 
                SurveyYear == 2022, 
                CharacteristicCategory == "Region")
