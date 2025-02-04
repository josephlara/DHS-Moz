

#' Create a list of indicators based on the level1 field in the dhs_indicators function
#'
#' @param level_1 level1 indicators wanted.  If "all" then all indicators are returned.  The default is all
#'
#' @return a list of indicators based on level1
#' @export
#'
#' @examples

create_indicator_list <- function(level_1 = "all"){
  
  all_indicators <- rdhs::dhs_indicators()
  
  if("all" %in% level_1){
    indicator_id <- all_indicators %>% 
      dplyr::select(indicator_id) %>% 
      dplyr::pull()
    return(indicator_id)
    
  }
  
  else{
    
    indicators <- all_indicators %>% 
      dplyr::filter(Level1 %in% level_1) %>% 
      dplyr::select(IndicatorId) %>% 
      dplyr::pull()  # Convert to vector
    
    return(indicators)
    
  }
  
}

#' Create an RDS dataset for Mozambique based on a given list of indicators and years
#'
#' @param indicator list of indicators
#' @param country country
#' @param start_year earliest year for surveys
#' @param end_year latest year for surveys
#'
#' @return a dataset of indicators and years
#' @export
#'
#' @examples
pull_indicator_df <- function(indicator, country = "MZ", breakdown_default = "all", 
                              start_year, end_year) {
  tryCatch({
    df <- rdhs::dhs_data(indicatorIds = indicator,
                         countryIds = country,
                         breakdown = breakdown_default,
                         surveyYearStart = start_year,
                         surveyYearEnd = end_year)
    
    if (nrow(df) == 0) {
      warning("No records returned for the given query.")
      return(NULL)
    }
    
    df <- df |>
      dplyr::filter(IsPreferred == 1,# in testing phase
                    
      ) |> 
      dplyr::select(CountryName, DHS_CountryCode, SurveyType, SurveyYear, Indicator, IndicatorId, CharacteristicCategory, CharacteristicLabel, Value, CIHigh, CILow) |>
      dplyr::mutate(CIHigh = ifelse(CIHigh == "", NA, CIHigh),
                    CILow = ifelse(CILow == "", NA, CILow))
    
    indicators <- rdhs::dhs_indicators()
    
    #TODO  check for duplication
    val_area <- indicators |>
      dplyr::select(IndicatorId, Level1) |>
      dplyr::distinct() 
    
    df <- df |>
      dplyr::left_join(val_area, by = "IndicatorId")
    
    
    
    return(df)
    
  }, error = function(e) {
    errorMessage <- paste("An error occurred:", conditionMessage(e))
    warning(errorMessage)
    return(NULL)
  })
}

#' Function to create a dataset from DHS_indicator() 
#'
#' @param start_idx starting index of a group of indicators.  Grouped by a batch_size
#' @param country countries - based on DHS_CountryCode
#' @param breakdown select: national, subnational, all, background
#' @param start start year of survey
#' @param end end year of survey
#'
#' @return a dataset with indicators
#' @export
#'
#' @examples

process_batch <- function(start_idx, batch_size, country, breakdown, start, end) {
  end_idx <- min(start_idx + batch_size - 1, total_indicators)
  batch_indicators <- indicators[start_idx:end_idx]
  
  pull_indicator_df(indicator = batch_indicators, 
                    country = country, 
                    breakdown_default = breakdown,
                    start_year = start, # specify start year
                    end_year = end)   # specify end year
}