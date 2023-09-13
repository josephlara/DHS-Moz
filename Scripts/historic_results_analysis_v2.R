
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
                              .default = NA)) %>% 
    select(country, survey, year, area, indicator, group, characteristic, snuuid, value)
  
  return(df)
  
}

sheets <- sheet_names(gs_id)

df1 <- load_dhs_data(sheetname = sheets[1])
df2 <- load_dhs_data(sheetname = sheets[2])
df3 <- load_dhs_data(sheetname = sheets[3])
df4 <- load_dhs_data(sheetname = sheets[4])
df5 <- load_dhs_data(sheetname = sheets[5])
df6 <- load_dhs_data(sheetname = sheets[6])
df7 <- load_dhs_data(sheetname = sheets[7])
df8 <- load_dhs_data(sheetname = sheets[8])
df9 <- load_dhs_data(sheetname = sheets[9])
df10 <- load_dhs_data(sheetname = sheets[10])
df11 <- load_dhs_data(sheetname = sheets[11])
df12 <- load_dhs_data(sheetname = sheets[12])
df13 <- load_dhs_data(sheetname = sheets[13])
df14 <- load_dhs_data(sheetname = sheets[14])
df15 <- load_dhs_data(sheetname = sheets[15])
df16 <- load_dhs_data(sheetname = sheets[16])
df17 <- load_dhs_data(sheetname = sheets[17])
df18 <- load_dhs_data(sheetname = sheets[18])


df_all <- bind_rows(df1,
                    df2,
                    df3,
                    df4,
                    df5,
                    df6,
                    df7,
                    df8,
                    df9,
                    df10,
                    df11,
                    df12,
                    df13,
                    df14,
                    df15,
                    df16,
                    df17,
                    df18)

write_csv(df_all, file = "Dataout/dhs_kir.csv")

saveRDS(df_all, "Dataout/dhs_kir.rds")
