library(tidyverse)

dhs_level_files <- dir("Dataout/pull/", pattern = "*.csv")

df_dhs_compile <- dhs_level_files %>% 
  map(~ read_csv(file.path("Dataout/pull/", .))) %>%
  reduce(rbind) |> 
  relocate(Level1, .before = Indicator) |> 
  relocate(ByVariableLabel, .before = Value) |> 
  relocate(PublicationURL, .after = SurveyYear)

write_csv(df_dhs_compile, "Dataout/dhs_compile_20240814.csv")
