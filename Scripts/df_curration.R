# PROJECT:  
# AUTHOR:   J. Lara | USAID
# PURPOSE:  
# REF ID:   1e3599e2 
# LICENSE:  MIT
# DATE:     2024-06-06
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
library(tidyverse)
library(janitor)
library(gt)
library(scales)
library(glue)
library(rdhs)
library(surgen)
library(googlesheets4)
library(glamr)
library(glitr)
library(viridis)
load_secrets()

# DATA IMPORT --------------------------------------------------------
  
df <- read_csv("Dataout/dhs_compile.csv")
dhs_tags <- dhs_tags()  # Tag Type: 0 = Subject groups, 1 = Indicator sets (e.g. SDGs, MDGs, PMI/RBM, IYCF, MICS), 2 = Select subsets of indicators
dhs_indicators <- dhs_indicators()

dhs_tags_menu <- dhs_tags |> 
  select(TagID, TagName) |> 
  arrange(TagID)

dhs_tags_join <- dhs_indicators |> 
  group_by(IndicatorId) |> 
  slice_head() |> 
  select(IndicatorId, Definition, TagIds)
  

df_1 <- df |> 
  left_join(dhs_tags_join, join_by(IndicatorId)) |> 
  relocate(Definition, .after = Indicator) |> 
  relocate(IndicatorId, .before = Indicator) |> 
  relocate(TagIds, .after = Definition) |> 
  glimpse()


# OUTPUT --------------------------------------------------------

write_csv(dhs_tags_menu,
          "Dataout/tag_menu.csv")

write_csv(df_1,
          "Dataout/survey_compile_moz.csv",
          na = "")
