# PROJECT:  
# AUTHOR:   J. Lara | USAID
# PURPOSE:  
# REF ID:   1fabc078 
# LICENSE:  MIT
# DATE:     2024-06-05
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
library(tidyverse)
library(janitor)
library(rdhs)
library(surgen)
library(glamr)
library(gt)
library(scales)
library(glue)
library(googlesheets4)
library(glitr)
library(viridis)
load_secrets()



# GLOBAL VARIABLES --------------------------------------------------------
  

# set rdhs api timeout
set_rdhs_config(timeout = 100)

# create a df of levels for which indicators are inventoried
levels <- dhs_indicators() |> distinct(Level1)

# set function argument inputs
LEVEL1 <- c("Survey Characteristics")
START <- 1995
END <- 2024
COUNTRY <- c("MZ")
BREAKDOWN <- "All"
BATCH_SIZE <- 50


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


# IMPORT ------------------------------------------------------------------


# inventory indicators for indicated "level"
indicators <- create_indicator_list(LEVEL1)

# count the number of indicators in indicated "level"
total_indicators <- length(indicators)

# create indices for batched indicator pulls
start_indices <- seq(1, 
                     total_indicators, 
                     by = BATCH_SIZE)

# batch download of indicator results for indicated "level"
df <- purrr::map(start_indices, ~ process_batch(.x, 
                                                BATCH_SIZE, 
                                                COUNTRY, 
                                                BREAKDOWN, 
                                                START,
                                                END)) |>
  dplyr::bind_rows()


# MUNGE -------------------------------------------------------------------


plot_nat_disagg(df, indicator = "SV_HRES_H_INT")
