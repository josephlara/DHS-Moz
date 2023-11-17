
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
library(rdhs)
load_secrets()


# QUERY INDICATORS --------------------------------------------------------


# "tags" describe the indicator area
# dhs_countries shows countries
# dhs_surveys shows dhs surveys conducted


ids <- dhs_indicators() %>% glimpse()

sc <- dhs_survey_characteristics()

survs <- dhs_surveys(surveyCharacteristicIds = 1, countryIds = c("MZ"), surveyYearStart = 1997)

datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "FL", fileType = "PR")

indicators <- dhs_indicators()

indicators %>% 
  filter(TagIds == 75) %>% 
  distinct()

tags <- dhs_tags()

tags[grepl("Malaria", tags$TagName), ]

indicators %>% 
  select(TagIds, Label, IndicatorId) %>% 
  filter(TagIds == 75)

test <- 
  indicators %>% 
  filter(TagIds == 75)




# interesting
# and now let's then grab this data by specifying the countryIds and the survey year starts
data <- dhs_data(indicatorIds = "CM_ECMT_C_U5M",
                 # characteristicLabel = "0-4",
                 countryIds = c("MZ"),
                 # breakdown = "all",
                 surveyYearStart = 1997) %>% 
  select(CountryName, SurveyType, SurveyYear, Indicator, Value, CIHigh, CILow) %>% 
  glimpse()


survs <- dhs_surveys(surveyCharacteristicIds = 89,
                     countryIds = c("MZ"),
                     surveyType = "DHS",
                     surveyYearStart = 1997)

# and lastly use this to find the datasets we will want to download and let's download the flat files (.dat) datasets (have a look in the dhs_datasets documentation for all argument options, and fileformat abbreviations etc.)
datasets <- dhs_datasets(surveyIds = survs$SurveyId, 
                         fileFormat = "flat")
data[1,]


data <- dhs_data(tagIds = 36, 
                 countryIds = c("MZ"),
                 breakdown="subnational",
                 surveyYearStart = 2018)


resp <- dhs_data(indicatorIds = "ML_FEVT_C_AML", 
                 surveyYearStart = 2010, 
                 breakdown = "subnational")

unique(resp$Indicator)

# filter it to 12 countries for space
countries_ssa  <- c("Uganda", "Kenya", "Tanzania",
                    "Rwanda","Zambia","Zimbabwe","Namibia",
                    "Mozambique")

# and plot the results
ggplot(resp[resp$CountryName %in% countries_ssa,],
       aes(x = SurveyYear,
           y = Value,
           colour = CountryName)) +
  geom_point() +
  geom_smooth(method = "glm") + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
  ylab(resp$Indicator[1]) + 
  facet_wrap(~ CountryName, 
             ncol = 4) 



test <- dhs_data(tagIds = 75, 
                 countryIds = c("MZ"),
                 breakdown="subnational",
                 surveyYearStart = 1997)
