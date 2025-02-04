# PURPOSE:  DHS API Data Pull
# AUTHOR:  Joe Lara | USAID
# DATE: 2022-05-12
# NOTES:

# DEPENDENCIES ------------------------

library(tidyverse)
library(readxl)
library(rdhs)
library(readr)
source("Scripts/utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------

set_rdhs_config(timeout = 100)  #need your api token for this

LEVEL1 <- c("Women's Empowerment")
START <- 2000
END <- 2022
COUNTRY <- c("MZ")
BREAKDOWN <- "All"
BATCH_SIZE <- 50

# Create dataset from api --------------------------------------------------------------------------------------------------------------

# Example when indicators are taken based on a Level

indicators <- create_indicator_list(LEVEL1)
total_indicators <- length(indicators)

# Create a sequence of start indices
start_indices <- seq(1, total_indicators, by = BATCH_SIZE)

result_from_indicators <- purrr::map(start_indices, 
                                     ~ process_batch(.x, 
                                                     BATCH_SIZE, 
                                                     COUNTRY, 
                                                     BREAKDOWN, 
                                                     START,
                                                     END)) %>%
  dplyr::bind_rows()



result_from_indicators %>%
  readr::write_csv(glue::glue("Dataout/pull/dhs_data_{LEVEL1}.csv"))


# Create dataset from files --------------------------------------------------------------------------------------------------------------

# Example when indicators come from a file  
indicator_in_file <- readxl::read_xlsx("Data/indicator_list.xlsx") 

indicators_from_file <- indicator_in_file %>% 
  dplyr::select(indicatorId) %>% 
  dplyr::distinct() %>% 
  dplyr::pull()

total_indicators <- length(indicators)
# Create a sequence of start indices
start_indices <- seq(1, total_indicators, by = BATCH_SIZE)

df_file <- purrr::map(start_indices, ~ process_batch(.x, BATCH_SIZE, COUNTRY, BREAKDOWN, START,
                                                     END)) %>%
  dplyr::bind_rows() %>% 
  readr::write_csv("Dataout/dhs_from_file.csv")


# CHECKS----------------------------------------------------------------------
#Why are there duplicates?  and where are they? 
check <- rdhs::dhs_indicators() %>% 
  dplyr::group_by(IndicatorId, MeasurementType) %>%
  dplyr::summarize(count = n()) %>% 
  dplyr::filter(count > 1) %>% 
  dplyr::pull(IndicatorId)


duplicates <- rdhs::dhs_indicators() %>% 
  dplyr::filter(IndicatorId %in% check) %>% 
  readr::write_csv("Dataout/duplicates.csv")



# COMPILE API FILES -------------------------------------------------------

all_dgs_input_files <- dir("Dataout/pull/api_pull/",
                           full.name = TRUE,
                           pattern = "*.csv")

df_compile <- purrr::map(all_dgs_input_files, ~ read_csv(.x)) %>% 
  bind_rows() |> 
  relocate(Level1, .before = SurveyYear) |> 
  write_csv("Dataout/pull/compile/dhs_data_compiled.csv")

