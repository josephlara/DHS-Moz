
# PURPOSE:  Munge and Analysis of DHS Results
# AUTHOR:  Joe Lara | USAID
# DATE: 2022-05-12
# NOTES: 

rm(list = ls())

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
library(patchwork)
library(viridis)
load_secrets()


gs_id <- as_sheets_id("1fR9HmYCctPCpMJ5fhBCaljF46x-WQVnMqeooj_tNCM0")


load_clean_province_long <- function(sheetname) {
  
  df <- read_sheet(as_sheets_id(gs_id), 
                   sheet = sheetname)  %>% 
    clean_names()
  
  colnames(df)[ncol(df)] <- "value"
  
  df <- df %>% 
    mutate(characteristic = str_remove_all(characteristic, "Provinces : |L1|\\(|\\)"),
           characteristic = str_remove_all(characteristic, "Residence : |L1|\\(|\\)"),
           characteristic = str_trim(characteristic, side = "right"),
           characteristic = str_remove_all(characteristic, " 15-49"),
           indicator = sheetname,
           value = value / 100) %>% 
    # separate(survey, sep = " ", c("year", "temp"), remove = FALSE) %>% 
    separate(survey, sep = 4, c("year", "temp"), remove = FALSE) %>% 
    mutate(year = as.numeric(year)) %>% 
    select(!temp) %>% 
    relocate(value, .after = everything())
  
  return(df)
  
}



df <- read_sheet(as_sheets_id(gs_id), 
                                  sheet = "Gravidez e maternidade na adolescência") %>% 
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
         characteristic = str_trim(characteristic, side = "right")) %>% 
  select(country, survey, year, indicator, group, characteristic, value) %>% 
  glimpse()
  

