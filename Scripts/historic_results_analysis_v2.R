
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
load_secrets()


# GLOBALS -----------------------------------------------------------------


gs_id <- as_sheets_id("1fR9HmYCctPCpMJ5fhBCaljF46x-WQVnMqeooj_tNCM0") # define sheet id

sheets <- sheet_names(gs_id) # get the names of individual sheets 

load_dhs_data <- function(sheetname) {
  
  df <- read_sheet(as_sheets_id(gs_id), 
                   sheet = sheetname) %>% 
    pivot_longer(where(is.numeric), names_to = "indicator", values_to = "value") %>% 
    filter(!is.na(value)) %>% 
    clean_names() %>% 
    separate_wider_delim(cols = characteristic, delim = " : ", names = c("group", "characteristic"),
                         too_few = "align_start") %>% 
    separate(survey, sep = 4, c("year", "temp"), remove = FALSE) %>% 
    select(!temp) %>% 
    mutate(year = as.numeric(year),
           characteristic = str_remove_all(characteristic, "Provinces : |L1|\\(|\\)"),
           characteristic = str_remove_all(characteristic, "Residence : |L1|\\(|\\)"),
           characteristic = str_remove_all(characteristic, "Living children: "),
           group = str_remove_all(group, " \\(5-year groups\\)"),
           group = str_remove_all(group, " 15-49"),
           characteristic = str_trim(characteristic, side = "right"),
           snuuid = case_when(country == "Mozambique" & characteristic == "Niassa" ~ "Oi0uBOAAVj9",
                              country == "Mozambique" & characteristic == "Cabo Delgado" ~ "Zpnu5qr8aoR",
                              country == "Mozambique" & characteristic == "Nampula" ~ "hxMhtp0apCm",
                              country == "Mozambique" & characteristic == "Zambézia" ~ "BdBJXnCZRJ5",
                              country == "Mozambique" & characteristic == "Tete" ~ "TAW0CceBfZ5",
                              country == "Mozambique" & characteristic == "Manica" ~ "aoD09rD9W63",
                              country == "Mozambique" & characteristic == "Sofala" ~ "SEWXP7RQHGN",
                              country == "Mozambique" & characteristic == "Inhambane" ~ "VESMH20BX4e",
                              country == "Mozambique" & characteristic == "Gaza" ~ "sAFsng0gK1E",
                              country == "Mozambique" & characteristic == "Maputo Provincia" ~ "YOJg1GA3qHT",
                              country == "Mozambique" & characteristic == "Maputo Cidade" ~ "NCUTZ4cYJra",
                              .default = NA),
          # new needing testing
           value = ifelse(!indicator %in% c("Total fertility rate 15-49",
                                            "Neonatal mortality rate 5 year periods",
                                            "Infant mortality rate 5 year periods",
                                            "Under-five mortality rate 5 year periods",
                                            "Mean height for age of children",
                                            "Mean weight for height of children",
                                            "Mean weight for age of children",
                                            "Mean number of sexual partners in lifetime [Women]",
                                            "Mean number of sexual partners in lifetime [Men]"),
                          value / 100,
                          value),
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
                              .default = group)) %>% 
    filter(country != "Mozambique" & group == "Total" | country == "Mozambique") %>% 
    select(country, survey, year, area, indicator, group, characteristic, snuuid, value)
  
  return(df)
  
} # function to pull and tidy dhs data

tidy_dhs_data <- function(df) {
  
  df <- df |> 
    pivot_longer(where(is.numeric), names_to = "indicator", values_to = "value") %>% 
    filter(!is.na(value)) %>% 
    clean_names() %>% 
    separate_wider_delim(cols = characteristic, delim = " : ", names = c("group", "characteristic"),
                         too_few = "align_start") %>% 
    separate(survey, sep = 4, c("year", "temp"), remove = FALSE) %>% 
    select(!temp) %>% 
    mutate(year = as.numeric(year),
           characteristic = str_remove_all(characteristic, "Provinces : |L1|\\(|\\)"),
           characteristic = str_remove_all(characteristic, "Residence : |L1|\\(|\\)"),
           characteristic = str_remove_all(characteristic, "Living children: "),
           group = str_remove_all(group, " \\(5-year groups\\)"),
           group = str_remove_all(group, " 15-49"),
           characteristic = str_trim(characteristic, side = "right"),
           snuuid = case_when(country == "Mozambique" & characteristic == "Niassa" ~ "Oi0uBOAAVj9",
                              country == "Mozambique" & characteristic == "Cabo Delgado" ~ "Zpnu5qr8aoR",
                              country == "Mozambique" & characteristic == "Nampula" ~ "hxMhtp0apCm",
                              country == "Mozambique" & characteristic == "Zambézia" ~ "BdBJXnCZRJ5",
                              country == "Mozambique" & characteristic == "Tete" ~ "TAW0CceBfZ5",
                              country == "Mozambique" & characteristic == "Manica" ~ "aoD09rD9W63",
                              country == "Mozambique" & characteristic == "Sofala" ~ "SEWXP7RQHGN",
                              country == "Mozambique" & characteristic == "Inhambane" ~ "VESMH20BX4e",
                              country == "Mozambique" & characteristic == "Gaza" ~ "sAFsng0gK1E",
                              country == "Mozambique" & characteristic == "Maputo Provincia" ~ "YOJg1GA3qHT",
                              country == "Mozambique" & characteristic == "Maputo Cidade" ~ "NCUTZ4cYJra",
                              .default = NA),
           # new needing testing
           value = ifelse(!indicator %in% c("Total fertility rate 15-49",
                                            "Neonatal mortality rate 5 year periods",
                                            "Infant mortality rate 5 year periods",
                                            "Under-five mortality rate 5 year periods",
                                            "Mean height for age of children",
                                            "Mean weight for height of children",
                                            "Mean weight for age of children",
                                            "Mean number of sexual partners in lifetime [Women]",
                                            "Mean number of sexual partners in lifetime [Men]"),
                          value / 100,
                          value),
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
                              .default = group)) %>% 
    filter(country != "Mozambique" & group == "Total" | country == "Mozambique") %>% 
    select(country, survey, year, area, indicator, group, characteristic, snuuid, value)
  
  return(df)
  
} # function to tidy dhs data

subset_dhs_data <- function(df, disaggregate = "Total", geography = NULL) {
  
  df1 <- df |> 
    filter(group %in% disaggregate)
  
  if (missing(geography)) df1
  else df1 |> filter(country %in% geography)

  
} # function to subset dhs dataframe

barchart_vertical_1yr <- function(df, yr_survey, var_indicator, disag, tx_title, tx_caption) {
  df |> 
    filter(country == "Mozambique",
           survey == yr_survey,
           indicator == var_indicator,
           group == disag) |>
    ggplot(aes(x = fct_reorder(characteristic, value), 
               y = value, 
               fill = value)) +
    geom_col(width = 0.75) +
    coord_flip() +
    scale_y_continuous(labels = percent) +
    scale_fill_gradient(low = "blue", high = "red") +
    theme(
      plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
      panel.spacing = unit(.75, "cm"),
      panel.grid.major.y = element_blank(),
      axis.text.x = element_text(size = 10, vjust = 0, hjust = .5),
      axis.text.y = element_text(size = 10, vjust = .5, hjust = 1),
      plot.title = element_text(
        size = 15, 
        hjust = 0,
        vjust = 1,
        color = "grey30"),
      plot.subtitle = element_text(
        face = "italic", 
        size = 7, 
        vjust = 6, 
        color = "grey30"),
      plot.caption = element_text(
        size = 9,
        color = "grey50"),
      legend.position = "none",
      legend.direction = "vertical",
      legend.title = element_blank()
    ) +
    geom_text(aes(label = percent(value, 1)), 
              hjust = 1.5, 
              size = 3.5,
              colour = "white") +
    labs(x = "",
         y = "",
         title = tx_title,
         caption = tx_caption)
}


# FETCH & MUNGE DATA ------------------------------------------------------


dhs_dfs <- map(sheets, .f = \(x) read_sheet(gs_id, sheet = x)) |> 
  list_rbind() |> 
  tidy_dhs_data()


# ANALYZE INDICATOR DATA --------------------------------------------------


dhs_dfs |> 
  barchart_vertical_1yr(yr_survey = "2022 DHS",
                        var_indicator = "Women with any anemia",
                        disag = "Provinces",
                        tx_title = "Women with any anemia",
                        tx_caption = "Source: 2022 DHS Key Indicator Report")


dhs_dfs |> 
  filter(country == "Mozambique", 
         indicator == "Place of delivery: Health facility", # input
         group == "Residence") |> # input
  ggplot(aes(year, value, color = characteristic)) + 
  geom_line(linewidth = 1, alpha = .75) +
  geom_point() +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  theme(
    panel.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
    plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80",
                                      size = 0.25,
                                      linetype = 1),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      face = "bold",
      size = 14, 
      hjust = 0,
      vjust = 1,
      color = "grey30"),
    plot.subtitle = element_text(
      face = "italic", 
      size = 7, 
      vjust = 6, 
      color = "grey30"),
    plot.caption = element_text(
      size = 9,
      color = "grey50",
      hjust = 0),
    legend.title = element_blank(),
    legend.text=element_text(size = 11),
    legend.position = "right",
    legend.direction = "vertical",
    legend.background = element_rect(fill = "#e7e7e5"),
    legend.key = element_rect(fill = "#e7e7e5"),
  ) +
  geom_text(aes(label = percent(value, 1)), 
            vjust = -1, 
            size = 3.5) +
  labs(x = "",
       y = "",
       title = "Place of delivery: Health facility", # input
       caption = "Sources:\n2022 DHS Key Indicator Report;\nhttps://www.statcompiler.com") #input 


# SANDBOX -----------------------------------------------------------------


test_levels <- unique(dhs_dfs$indicator)


# WRITE TO DISK -----------------------------------------------------------


write_csv(df_all, file = "Dataout/dhs_kir.csv")

saveRDS(df_all, "Dataout/dhs_kir.rds")

