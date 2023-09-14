
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

df <- readRDS("Dataout/dhs_kir.rds")

indicators <- distinct(df, area, indicator)
areas <- distinct(df, area)

df2 <- df %>% 
  filter(country != "Mozambique" & group == "Total" | country == "Mozambique") %>% 
  mutate(value = ifelse(!indicator %in% c("Total fertility rate 15-49",
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
                            .default = group))

distinct(df2, group) %>% 
  arrange(group) %>% 
  print(n=100)

df2 %>% 
  distinct(country, group) %>% 
  print(n=100)


  
df %>% 
  filter(country == "Mozambique",
         is.na(group))

# define characteristic for Early Child Mortality in GoogleSheets.  Make it "Total" DONE
# change indicator name for malaria prevalence in GoogleSheets to "Malaria prevalence according to RDT"
