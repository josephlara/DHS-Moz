
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
library(glue)
library(scales)
library(ggthemes)
library(fs)
library(patchwork)
library(viridis)
library(rdhs)
load_secrets()

# DATA REFERENCE ----------------------------------------------------------

tags <- rdhs::dhs_tags() |> arrange(TagID)
indicators <- rdhs::dhs_indicators()

# FUNCTIONS ---------------------------------------------------------------

# grab and save indicator data for single year with all available disaggregations
save_nat_disagg <- function(indicator, country = "MZ", survey_year) {
  
  df <- rdhs::dhs_data(indicatorIds = indicator,
                       countryIds = country,
                       surveyYear	= survey_year,
                       breakdown = "all") |> 
    dplyr::filter(IsPreferred == 1) |> # in testing phase
    dplyr::select(CountryName, SurveyType, SurveyYear, Indicator, IndicatorId, CharacteristicCategory, CharacteristicLabel, Value, CIHigh, CILow) |>
    dplyr::mutate(CIHigh = ifelse(CIHigh == "", NA, CIHigh),
                  CILow = ifelse(CILow == "", NA, CILow))
  
  indicators <- rdhs::dhs_indicators()
  
  val_area <- indicators |> 
    dplyr::filter(IndicatorId == indicator) |> 
    dplyr::distinct(Level1) |> 
    dplyr::pull()
  val_indicator <- df |> dplyr::distinct(Indicator) |> pull()
  val_filename <- glue::glue('Data/{val_area} - {val_indicator}.csv')
  
  df |> 
    write_excel_csv(file = val_filename,
                    append = TRUE)
  
}








save_nat_disagg <- function(indicator, country = "MZ", survey_year) {
  
  df <- rdhs::dhs_data(indicatorIds = indicator,
                       countryIds = country,
                       surveyYear	= survey_year,
                       breakdown = "all") |> 
    dplyr::filter(IsPreferred == 1) |> # in testing phase
    dplyr::select(CountryName, SurveyType, SurveyYear, Indicator, IndicatorId, CharacteristicCategory, CharacteristicLabel, Value, CIHigh, CILow) |>
    dplyr::mutate(CIHigh = ifelse(CIHigh == "", NA, CIHigh),
                  CILow = ifelse(CILow == "", NA, CILow))
  
  indicators <- rdhs::dhs_indicators()
  
  val_area <- indicators |> 
    dplyr::filter(IndicatorId == indicator) |> 
    dplyr::distinct(Level1) |> 
    dplyr::pull()
  val_indicator <- df |> dplyr::distinct(Indicator) |> pull()
  val_filename <- glue::glue('Data/{val_area} - {val_indicator}.csv')
  
  return(df)
  
}







grab_nat_disagg <- function(indicator, country = "MZ", survey_year, breakdown) {
  
  df <- rdhs::dhs_data(indicatorIds = indicator,
                       countryIds = country,
                       surveyYear	= survey_year,
                       breakdown = "all") |> 
    dplyr::filter(IsPreferred == 1) |> # in testing phase
    dplyr::select(CountryName, SurveyType, SurveyYear, Indicator, CharacteristicCategory, CharacteristicLabel, CharacteristicId, Value, CIHigh, CILow) |> 
    dplyr::filter(CharacteristicCategory == breakdown) %>% 
    dplyr::mutate(CIHigh = ifelse(CIHigh == "", NA, CIHigh),
                  CILow = ifelse(CILow == "", NA, CILow))
  
  
  return(df)
  
}

list_indicator <- c("ML_PMAL_C_RDT", "CH_VACC_C_DP3")
list_year <- c("1997", "2003", "2011", "2015", "2018", "2022")

data_df <- map(
  list_indicator, ~ save_nat_disagg(indicator = .x, survey_year = list_year)) %>%
  bind_rows()


list_indicator <- c("ML_PMAL_C_RDT", "CH_VACC_C_DP3")

map_df(.x = list_indicator,
       .f = ~save_nat_disagg(.x, country = "MZ", 2022))



# works

df <- map_df(.x = list_indicator,
       .f = ~ save_nat_disagg(.x, country = "MZ", survey_year = list_year))


list_indicator <- 
  c("ML_NETP_H_ITN",
    "ML_NETP_H_IT2",
    "ML_ITNA_P_ACC",
    "ML_NETU_P_ITN",
    "ML_NETC_C_ITN",
    "ML_NETC_C_IT1",
    "ML_NETW_W_ITN",
    "ML_NETW_W_IT1",
    "ML_IPTP_W_3SP",
    "ML_IPTP_W_2SA",
    "ML_FEVT_C_ADV",
    "ML_AMLD_C_ACT",
    "ML_HEMO_C_HL8",
    "ML_PMAL_C_RDT",
    "ML_PMAL_C_MSY",
    "MA_MBAG_W_B15",
    "MA_MBAG_W_B18",
    "MA_MBAG_M_B15",
    "MA_MBAG_M_B18",
    "WE_OWNA_W_LNO",
    "WE_OWNA_M_LNO",
    "WE_DMAK_W_3DC",
    "WE_AWBT_W_AGR",
    "WE_AWBT_M_AGR",
    "DV_SPV1_W_POS",
    "DV_SPV1_W_ANY",
    # "FG_PFCC_W_WCC"
    "EM_EMPM_W_EMP",
    "EM_EMPM_M_EMP",
    "FP_CUSM_W_ANY",
    "FP_CUSM_W_MOD", 
    # "FP_CUSM_W_TFK",
    "FP_SRCM_W_PUB",
    "FP_SRCM_W_PRV", # ok
    # "FP_ICHC_W_SID",
    # "FP_DISR_W_PRG",
    "FP_DISR_W_ANY") # ok
    "FP_KFTP_W_HLF",
    "FP_NADM_W_UNT",
    "FP_NADM_W_MNT",
    # "FP_NADM_W_TDT", # timeout
    "FP_NADM_W_PDM",
    "FP_FUTU_W_I12",
    "CN_NUTS_C_HA2",
    "CN_NUTS_C_WH2",
    "CN_NUTS_C_WHP",
    "CN_BRFI_C_1HR",
    "CN_BRFS_C_EXB",
    "CN_BRFS_C_BRF",
    "CN_IYCF_C_BTB",
    "CN_IYCF_C_3PN",
    "CN_IODZ_H_IOD",
    "AN_NUTS_W_THN",
    "AN_NUTS_W_OWT",
    "AN_NUTS_M_THN",
    "AN_NUTS_M_OWT",
    "AN_MIAM_W_90P",
    "CM_ECMR_C_NNR",
    "CM_ECMR_C_PNR",
    "CM_ECMR_C_IMR",
    "CM_ECMR_C_CMR",
    "CM_ECMR_C_U5M",
    "ED_EDUC_W_NED",
    "ED_EDUC_W_PRI",
    "ED_EDUC_W_SEH",
    "ED_EDUC_M_NED",
    "ED_EDUC_M_PRI",
    "ED_EDUC_M_SEH",
    "ED_LITR_W_LIT",
    "ED_LITR_M_LIT")

list_indicator <-
  c("ML_NETP_H_ITN",
    "ML_NETP_H_IT2",
    "ML_ITNA_P_ACC",
    "ML_NETU_P_ITN",
    "ML_NETC_C_ITN",
    "ML_NETC_C_IT1",
    "ML_NETW_W_ITN",
    "ML_NETW_W_IT1",
    "ML_IPTP_W_3SP",
    "ML_IPTP_W_2SA",
    "ML_FEVT_C_ADV",
    "ML_AMLD_C_ACT",
    "ML_HEMO_C_HL8",
    "ML_PMAL_C_RDT",
    "ML_PMAL_C_MSY",
    "MA_MBAG_W_B15",
    "MA_MBAG_W_B18",
    "MA_MBAG_M_B15",
    "MA_MBAG_M_B18",
    "WE_OWNA_W_LNO",
    "WE_OWNA_M_LNO",
    "WE_DMAK_W_3DC",
    "WE_AWBT_W_AGR",
    "WE_AWBT_M_AGR",
    "DV_SPV1_W_POS",
    "DV_SPV1_W_ANY",
    "FG_PFCC_W_WCC",
    "EM_EMPM_W_EMP",
    "EM_EMPM_M_EMP",
    "FP_CUSM_W_ANY",
    "FP_CUSM_W_MOD",
    "FP_CUSM_W_TFK",
    "FP_SRCM_W_PUB",
    "FP_SRCM_W_PRV", # ok
    "FP_ICHC_W_SID",
    "FP_DISR_W_PRG",
    "FP_DISR_W_ANY",
    "FP_KFTP_W_HLF",
    "FP_NADM_W_UNT",
    "FP_NADM_W_MNT",
    "FP_NADM_W_TDT",
    "FP_NADM_W_PDM",
    "FP_FUTU_W_I12",
    "CN_NUTS_C_HA2",
    "CN_NUTS_C_WH2",
    "CN_NUTS_C_WHP",
    "CN_BRFI_C_1HR",
    "CN_BRFS_C_EXB",
    "CN_BRFS_C_BRF",
    "CN_IYCF_C_BTB",
    "CN_IYCF_C_3PN",
    "CN_IODZ_H_IOD",
    "AN_NUTS_W_THN",
    "AN_NUTS_W_OWT",
    "AN_NUTS_M_THN",
    "AN_NUTS_M_OWT",
    "AN_MIAM_W_90P",
    "CM_ECMR_C_NNR",
    "CM_ECMR_C_PNR",
    "CM_ECMR_C_IMR",
    "CM_ECMR_C_CMR",
    "CM_ECMR_C_U5M",
    "ED_EDUC_W_NED",
    "ED_EDUC_W_PRI",
    "ED_EDUC_W_SEH",
    "ED_EDUC_M_NED",
    "ED_EDUC_M_PRI",
    "ED_EDUC_M_SEH",
    "ED_LITR_W_LIT",
    "ED_LITR_M_LIT")


save_nat_disagg <- function(indicator, country = "MZ", survey_year) {
  tryCatch({
    df <- rdhs::dhs_data(indicatorIds = indicator,
                         countryIds = country,
                         surveyYear = survey_year,
                         breakdown = "all")
    
    if (nrow(df) == 0) {
      warning("No records returned for the given query.")
      return(NULL)
    }
    
    df <- df |>
      dplyr::filter(IsPreferred == 1) |> # in testing phase
      dplyr::select(CountryName, SurveyType, SurveyYear, Indicator, IndicatorId, CharacteristicCategory, CharacteristicLabel, Value, CIHigh, CILow) |>
      dplyr::mutate(CIHigh = ifelse(CIHigh == "", NA, CIHigh),
                    CILow = ifelse(CILow == "", NA, CILow))
    
    indicators <- rdhs::dhs_indicators()
    
    val_area <- indicators |>
      dplyr::filter(IndicatorId == indicator) |>
      dplyr::distinct(Level1) |>
      dplyr::pull()
    val_indicator <- df |> dplyr::distinct(Indicator) |> pull()
    val_filename <- glue::glue('Data/{val_area} - {val_indicator}.csv')
    
    return(df)
    
  }, error = function(e) {
    errorMessage <- paste("An error occurred:", conditionMessage(e))
    warning(errorMessage)
    return(NULL)
  })
}
df <- map_df(.x = list_indicator,
             .f = ~ save_nat_disagg(.x, country = "MZ", survey_year = list_year))

