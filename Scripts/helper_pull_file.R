# DEPENDENCIES ------------------------

library(tidyverse)
library(googlesheets4)
library(readxl)
library(openxlsx)
library(rdhs)
load_secrets()



tags <- rdhs::dhs_tags() |> arrange(TagID)
indicators <- rdhs::dhs_indicators()
pubs <- rdhs::dhs_publications()
surveys <- rdhs::dhs_surveys()

data <- rdhs::dhs_data(indicatorIds = "CN_NUTS_C_HA2",
                     countryIds = "MZ",
                     surveyYearStart = 1900,
                     breakdown = "all")

df <- as_tibble(list_indicator) |>
  left_join(indicators, join_by(value == IndicatorId)) |> 
  select("IndicatorId" = value,
         Level1,
         Level2,
         ShortName,
         Label,
         Denominator,
         Definition)

write_csv(
  df,
  "Documents/IndicatorID_pull.csv"
)


indicators_add <- indicators |> 
  filter(Level1 == "Reproductive Health") |> 
  select(IndicatorId,
         Level1,
         Level2,
         ShortName,
         Label,
         Denominator,
         Definition)






# df_level <- indicators |> 
#   dplyr::select(IndicatorID, Level1)

# df <- df |> 
#   dplyr::left_join(df_level, join_by(IndicatorId = IndicatorId)) |> 
#   dplyr::relocate(Level1, .before = Indicator)