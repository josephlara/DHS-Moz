
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


gs_id <- as_sheets_id("1c1Xpaefm9LRrocGHUvZHyK8Y1-j6gEe3klFvf72U7_E")

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
  

# Population Growth -------------------------------------------------------

df_demproj <- read_csv("Data/ine_demproj.csv")

df_demproj_prov <- df_demproj %>%
  summarize(
    value = sum(value, na.rm=T),
    .by = c(year, snu) 
  )


df_demproj_prov %>% 
  ggplot(aes(year, value, color = snu)) +
  geom_line(size = 1) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 9),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = snu),
            data = df_demproj_prov %>% filter(year == "2050", snu %in% c("Nampula", "Zambezia", "Tete")),
            size = 3.5,
            vjust = -.75,
            hjust = .5,
            nudge_y = 0, 
            nudge_x = -2) +
  scale_x_continuous(breaks = seq(from = 2020, to = 2050, by = 5)) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M", accuracy = 1)) +
  labs(x = "",
       y = "Population Size (Millions)",
       title = "Mozambique Demographic Projections by Province",
       subtitle = "Demographic projections generated subsequent to 2017 Census",
       caption = "Source: INE Demographic Projections")

# Fertility Rate ---------------------------------------------------------------

df_fertility <- load_clean_province_long("Total Fertility Rate")


df_fertility %>% 
  mutate(value = value * 100) %>% 
  ggplot(aes(year, value, color = characteristic)) + 
  geom_line(size = 1) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = scales::number(value, accuracy = 0.1)),
            size = 3.5,
            vjust = -.6, 
            hjust = .5) +
  scale_y_continuous(labels = number,
                     limits = c(3, 7)) + 
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2015, 2018, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'IMASIDA 2015', 'MIS 2018', 'DHS 2022')) +
  labs(x = "",
       y = "",
       title = "Total Fertility Rate by Residence",
       subtitle = "Total fertility rate for the three years preceding the survey for wowen 15-49 years (expressed per woman)",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 13")


# Fertility Rate Region ---------------------------------------------------

df_fertility_reg <- load_clean_province_long("Total Fertility Rate Region") %>% 
  mutate(value = value * 100)

unique(df_fertility_reg$country)
country_filter <- c("Mozambique", "Angola", "South Africa", "Zambia", "Zimbabwe", "Tanzania", "Kenya", "Uganda", "Lesotho", "Nambia", "Malawi", "Rwanda")

df_mortality_moz <- df_mortality_region %>% filter(country == "Mozambique")

df_fertility_reg %>% 
  filter(country %in% country_filter) %>% 
  ggplot(aes(year, value, color = country)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = country),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks = c(1980:2022)) +
  labs(x = "",
       y = "",
       title = "Total Fertility Rate by Country (1997-2022)",
       subtitle = "Total fertility rate for the three years preceding the survey for wowen 15-49 years (expressed per woman)",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 13")


# Adolescent Pregnancy ----------------------------------------------------

df_adol_mothers <- load_clean_province_long("Teenagers who are mothers")

# National Trend by Urban/Rural
df_adol_mothers %>% 
  filter(characteristic %in% c("Total", "Urban", "Rural")) %>% 
  ggplot(aes(year, value, color = characteristic)) + 
  geom_line(size = 1) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  scale_fill_discrete(breaks = c('Total', 'Urban', 'Rural')) +
  geom_text(aes(label = scales::percent(value, 1)),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2015, 2018, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'IMASIDA 2015', 'MIS 2018', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, .5)) + 
  labs(x = "",
       y = "",
       title = "Teenagers who are mothers",
       subtitle = "Percentage of teenage women who are mothers",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 13")

# Two year comparison
df_adol_mothers %>% 
  filter(!characteristic %in% c("Total", "Urban", "Rural"),
         year %in% c(2011, 2022)) %>% 
  mutate(year = as.character(year)) %>% 
  ggplot(aes(characteristic, value, fill = year)) + 
  geom_col(position = "dodge") +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  scale_fill_discrete(breaks = c('Total', 'Urban', 'Rural')) +
  geom_text(aes(label = scales::percent(value, 1)),
            size = 3.5,
            vjust = 0, 
            hjust = 0) +
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2015, 2018, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'IMASIDA 2015', 'MIS 2018', 'DHS 2022')) +
  scale_y_continuous(label = scales::percent(value, 1),
                     limits = c(0, .5)) + 
  labs(x = "",
       y = "",
       title = "Teenagers who are mothers",
       subtitle = "Percentage of teenage women who are mothers",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 13")

  
# Subanalysis of Pregnancy and Maternity
df_pregnancy <- load_clean_province_long("Pregnancy History") %>% 
  select(!c(indicator, value)) %>% 
  pivot_longer(has_had_live_birth:has_been_pregnant, names_to = "indicator", values_to = "value") %>% 
  mutate(indicator = case_when(indicator == "has_been_pregnant" ~ "Has been pregnant",
                               indicator == "has_had_live_birth" ~ "Has had live birth",
                               indicator == "has_lost_a_pregnancy" ~ "Has lost a pregnancy",
                               indicator == "currently_pregnant" ~ "Currently pregnant"))

df_pregnancy %>% 
  filter(survey == "2022 DHS",
         characteristic != "Total") %>% 
  mutate(value = value / 100) %>% 
  ggplot(aes(x = fct_reorder(characteristic, value), y = value, fill = characteristic)) +
  geom_col(width = 0.75) +
  scale_y_continuous(labels = percent) + 
  theme_fivethirtyeight() +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 16, vjust = 2),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 8, color = "grey50"),
        plot.caption = element_text(size = 9),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = percent(value, 1)), 
            hjust = 1.5, 
            colour = "white") +
  labs(x = "",
       y = "",
       title = "Pregnancy & Maternity among Adolescents",
       subtitle = "
Percentage of women age 15–19 who have had a live birth, percentage of women who have never lost a pregnancy, percentage of women who are currently pregnant, \nand percentage of women who have been pregnant",
       caption = "Source: 2022 DHS Key Indicator Report, Page 13") +
  facet_wrap(~ factor(indicator, levels=c('Has been pregnant', 'Has had live birth', 'Currently pregnant', 'Has lost a pregnancy')), nrow = 1)


# Under 5 Mortality Rate ---------------------------------------------------------------

df_mortality <- load_clean_province_long("Gráfico 3 Tendências nas taxas de mortalidade") %>% 
  mutate(value = value * 100)

# Early Child Mortality Trend
df_mortality %>% 
  ggplot(aes(year, value, color = characteristic)) + 
  geom_line(size = 1) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  scale_fill_discrete(breaks = c('Under 5 Mortality', 'Infant Mortality', 'Neonatal Mortality')) +
  geom_text(aes(label = value),
            size = 3.5,
            vjust = -.5, 
            hjust = .5) +
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'DHS 2022')) +
  labs(x = "",
       y = "",
       title = "Early Childhood Mortality",
       subtitle = "Neonatal Mortality: Probability of dying in the first month of life per 1,000 live births \nInfant Mortality: Probability of dying before the first birthday per 1,000 live births\nUnder 5 Mortality: Probability of dying between the first birthday and the fifth birthday per 1,000 children surviving to their first birthday",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 19")



# Under 5 Mortality Rate Africa U5 ------------------------------------------------

df_mortality_region <- load_clean_province_long("Gráfico 3 Tendências Regionais mortalidade u5") %>% 
  mutate(value = value * 100)

unique(df_mortality_region$country)
country_filter <- c("Mozambique", "Angola", "South Africa", "Zambia", "Zimbabwe", "Tanzania", "Kenya", "Uganda", "Lesotho", "Nambia", "Malawi", "Rwanda")

df_mortality_moz <- df_mortality_region %>% filter(country == "Mozambique")

df_mortality_region %>% 
  filter(country %in% country_filter) %>% 
  ggplot(aes(year, value, color = country)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = country),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks = c(1980:2022)) +
  labs(x = "",
       y = "",
       title = "Under 5 Mortality by Country (1987-2022)",
       subtitle = "Under 5 Mortality: Probability of dying between the first birthday and the fifth birthday per 1,000 children surviving to their first birthday",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 19")


# Malaria Mosquito Nets ---------------------------------------------------------------------

df_malaria_itn_1 <- load_clean_province_long("Households with at least one ITN")
df_malaria_itn_2 <- load_clean_province_long("Households with at least one ITN 2 & Stayed")

df_malaria_itn <- bind_rows(df_malaria_itn_1, df_malaria_itn_2) %>% 
  mutate(indicator = case_when(indicator == "Households with at least one ITN" ~ "Households with at least one ITN",
                               .default = "Households with at least one ITN for every two persons who stayed in the household the previous night"))

# Mozambique National Trend
df_malaria_itn %>% 
  filter(country == "Mozambique",
         characteristic == "Total") %>% 
  ggplot(aes(year, value, color = indicator)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = scales::percent(value, 1)),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks=c(2011, 2015, 2018, 2022), labels=c('DHS 2011', 'IMASIDA 2015', 'MIS 2018', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Household Ownership and Coverage of ITN",
       subtitle = "* Percentage of households with at least one insecticide treated mosquito net (ITN)\n* Percentage of households with at least one mosquito net for every two persons who stayed in the \nhousehold the previous night",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 36")


# Mozambique Provincial Order
df_malaria_itn %>% 
  filter(survey == "2022 DHS",
         indicator == "Households with at least one ITN",
         characteristic != "Total") %>% 
  ggplot(aes(x = fct_reorder(characteristic, value), y = value, fill = characteristic)) +
  geom_col(width = 0.75) +
  scale_y_continuous(labels = percent) + 
  theme_fivethirtyeight() +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = percent(value, 1)), 
            hjust = 1.5, 
            colour = "white") +
  labs(x = "",
       y = "",
       title = "Ownership of ITN by Province (2022)",
       subtitle = "",
       caption = "Source: 2022 DHS Key Indicator Report, Page 36")

# Mozambique Provincial Trend
df_malaria_itn_moz <- df_malaria_itn %>% 
  filter(country == "Mozambique",
         indicator == "Households with at least one ITN",
         characteristic != "Total")

df_malaria_itn_moz %>% 
  ggplot(aes(year, value, color = characteristic)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_blank(),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = label_percent()(value)),
            data = df_malaria_itn_moz %>% filter(year == "2022"),
            size = 3,
            vjust = 1, 
            hjust = 1) +
  scale_x_continuous(breaks=c(2011, 2015, 2018, 2022), labels=c('DHS 2011', 'IMASIDA 2015', 'MIS 2018', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Ownership of ITN by Province",
       subtitle = "* Percentage of households with at least one insecticide treated mosquito net (ITN)",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 36") +
  facet_wrap(~ factor(characteristic, levels = c('Niassa', 'Cabo Delgado', 'Nampula', 'Zambézia', 'Tete', 'Manica', 'Sofala', 'Inhambane', 'Gaza', 'Maputo Provincia', 'Maputo Cidade')))




# Malaria Child / PW Mosquito Nets ---------------------------------------------------------------------


df_malaria_itn_pw <- load_clean_province_long("Pregnant women who slept under ITN")
df_malaria_itn_child <- load_clean_province_long("Children under 5 who slept under ITN")

df_malaria_itn_pwc <- bind_rows(df_malaria_itn_child, df_malaria_itn_pw)

# Mozambique National Trend
df_malaria_itn_pwc %>% 
  filter(country == "Mozambique",
         characteristic == "Total") %>% 
  ggplot(aes(year, value, color = indicator)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = scales::percent(value, 1)),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks=c(2011, 2015, 2018, 2022), labels=c('DHS 2011', 'IMASIDA 2015', 'MIS 2018', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "ITN use among Pregnant Women and Children",
       subtitle = "* Percentage of children under age five who slept under an ITN the night before the survey\n* 	Percentage of pregnant women who slept under an ITN the night before the survey",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 38")


# Mozambique Provincial Order
df_malaria_itn_pwc %>% 
  filter(survey == "2022 DHS",
         indicator == "Children under 5 who slept under ITN",
         !characteristic %in% c("Total", "Urban", "Rural")) %>% 
  ggplot(aes(x = fct_reorder(characteristic, value), y = value, fill = characteristic)) +
  geom_col(width = 0.75) +
  scale_y_continuous(labels = percent) + 
  theme_fivethirtyeight() +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = percent(value, 1)), 
            hjust = 1.5, 
            colour = "white") +
  labs(x = "",
       y = "",
       title = "ITN use among Children by Province (2022)",
       subtitle = "",
       caption = "Source: 2022 DHS Key Indicator Report, Page 38")

# Mozambique Provincial Trend
df_malaria_itn_moz <- df_malaria_itn %>% 
  filter(country == "Mozambique",
         indicator == "Households with at least one ITN",
         characteristic != "Total")

df_malaria_itn_moz %>% 
  ggplot(aes(year, value, color = characteristic)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_blank(),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = label_percent()(value)),
            data = df_malaria_itn_moz %>% filter(year == "2022"),
            size = 3,
            vjust = 1, 
            hjust = 1) +
  scale_x_continuous(breaks=c(2011, 2015, 2018, 2022), labels=c('DHS 2011', 'IMASIDA 2015', 'MIS 2018', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Households with at least one ITN by Province",
       subtitle = "* Percentage of households with at least one insecticide treated mosquito net (ITN)",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 36") +
  facet_wrap(~ factor(characteristic, levels = c('Niassa', 'Cabo Delgado', 'Nampula', 'Zambézia', 'Tete', 'Manica', 'Sofala', 'Inhambane', 'Gaza', 'Maputo Provincia', 'Maputo Cidade')))




# Malaria IPT -------------------------------------------------------------

df_malaria_ipt <- read_sheet(as_sheets_id(gs_id), 
                            sheet = "SP/F doses in pregnancy") %>% 
  clean_names() %>% 
  mutate(characteristic = str_remove_all(characteristic, "Provinces : |L1|\\(|\\)"),
         characteristic = str_remove_all(characteristic, "Residence : |L1|\\(|\\)"),
         characteristic = str_trim(characteristic, side = "right"),
         characteristic = str_remove_all(characteristic, " 15-49"),
         value = value / 100) %>% 
  separate(survey, sep = 4, c("year", "temp"), remove = FALSE) %>% 
  mutate(year = as.numeric(year)) %>% 
  select(!temp) %>% 
  relocate(value, .after = everything())


df_malaria_ipt %>% 
  filter(country == "Mozambique",
         characteristic == "Total") %>% 
  ggplot(aes(year, value, color = indicator)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = scales::percent(value, 1)),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks=c(2011, 2015, 2018, 2022), labels=c('DHS 2011', 'IMASIDA 2015', 'MIS 2018', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "SP/Fansidar use during pregnancy",
       subtitle = "* Percentage of women age 15-49 with a live birth in the two years \npreceding the survey who during the pregnancy took 1, 2, 3 dose(s) of SP/Fansidar",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 39")


# Mozambique Provincial Trend
df_malaria_ipt_moz <- df_malaria_ipt %>% 
  filter(country == "Mozambique",
         indicator == "SP/Fansidar 3+ doses during pregnancy",
         !characteristic %in% c("Total", "Rural", "Urban"))

df_malaria_ipt_moz %>% 
  ggplot(aes(year, value, color = characteristic)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_blank(),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = label_percent()(value)),
            data = df_malaria_ipt_moz %>% filter(year == "2022"),
            size = 3,
            vjust = 1, 
            hjust = 1) +
  scale_x_continuous(breaks=c(2011, 2015, 2018, 2022), labels=c('DHS 2011', 'IMASIDA 2015', 'MIS 2018', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "SP/Fansidar 3+ doses during pregnancy",
       subtitle = "* Percentage of women age 15-49 with a live birth in the two years preceding the survey who during the pregnancy \ntook 3rd dose of SP/Fansidar",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 39") +
  facet_wrap(~ factor(characteristic, levels = c('Niassa', 'Cabo Delgado', 'Nampula', 'Zambézia', 'Tete', 'Manica', 'Sofala', 'Inhambane', 'Gaza', 'Maputo Provincia', 'Maputo Cidade')))



# Malaria IPT/ANC 4 -------------------------------------------------------------

df_mch_anc4 <- load_clean_province_long("Antenatal visits for pregnancy: 4+ visits") %>% 
  filter(!characteristic %in% c("Total", "Rural", "Urban"),
         year > 2010) %>% 
  mutate(indicator = "ANC4")

df_malaria_ipt3 <- read_sheet(as_sheets_id(gs_id), 
                             sheet = "SP/F doses in pregnancy") %>% 
  clean_names() %>% 
  mutate(characteristic = str_remove_all(characteristic, "Provinces : |L1|\\(|\\)"),
         characteristic = str_remove_all(characteristic, "Residence : |L1|\\(|\\)"),
         characteristic = str_trim(characteristic, side = "right"),
         characteristic = str_remove_all(characteristic, " 15-49"),
         value = value / 100) %>% 
  separate(survey, sep = 4, c("year", "temp"), remove = FALSE) %>% 
  mutate(year = as.numeric(year)) %>% 
  select(!temp) %>% 
  relocate(value, .after = everything()) %>% 
  filter(country == "Mozambique",
         indicator == "SP/Fansidar 3+ doses during pregnancy",
         !characteristic %in% c("Total", "Rural", "Urban")) %>% 
  mutate(indicator = "SP3")
  

df_malaria_ipt3 %>% 
  ggplot(aes(year, value)) + 
  geom_line(size = 1, 
            color = "#F8766D",
            alpha = .75) +
  geom_line(data = df_mch_anc4,
            size = 1, 
            color = "#00BFC4",
            alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_blank(),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = indicator),
            data = df_malaria_ipt3 %>% filter(year == "2022"),
            size = 3,
            color = "#F8766D",
            vjust = 1, 
            hjust = 1) +
  geom_text(aes(label = indicator),
            data = df_mch_anc4 %>% filter(year == "2022"),
            size = 3,
            color = "#00BFC4",
            vjust = -1, 
            hjust = 1) +
  scale_x_continuous(breaks=c(2011, 2015, 2018, 2022), labels=c('DHS 2011', 'IMASIDA 2015', 'MIS 2018', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "SP/Fansidar 3 vs. ANC 4",
       subtitle = "* Percentage of women age 15-49 with a live birth in the two years preceding the survey who during the pregnancy took 3rd dose of SP/Fansidar\n* Percentage of women who had a live birth in the two years preceding the survey who had 4+ antenatal care visits",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 21, 39") +
  facet_wrap(~ factor(characteristic, levels = c('Niassa', 'Cabo Delgado', 'Nampula', 'Zambézia', 'Tete', 'Manica', 'Sofala', 'Inhambane', 'Gaza', 'Maputo Provincia', 'Maputo Cidade')))


# Malaria Prevalence ------------------------------------------------

df_malaria_prev <- load_clean_province_long("Regional Malaria prevalence according to RDT") %>% 
  mutate(value = value * 100)

unique(df_malaria_prev$country)
country_filter <- c("Mozambique", "Angola", "South Africa", "Zambia", "Zimbabwe", "Tanzania", "Kenya", "Uganda", "Lesotho", "Nambia", "Malawi", "Rwanda")

# Regional trend
df_malaria_prev %>% 
  filter(characteristic == "Total") %>% 
  mutate(value = value / 100) %>% 
  ggplot(aes(year, value, color = country)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = country),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks = c(1980:2022)) +
  scale_y_continuous(labels = percent,
                     limits = c(0, .8)) + 
  labs(x = "",
       y = "",
       title = "Malaria Prevalence in Children by Country (2006-2022)",
       subtitle = "Percentage of children age 6-59 months tested using a rapid diagnostic test (RDT) who are positive for malaria",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 42")


# Mozambique National Trend
df_malaria_prev %>% 
  filter(country == "Mozambique",
         characteristic == "Total") %>% 
  mutate(value = value / 100) %>% 
  ggplot(aes(year, value)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = scales::percent(value)),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks=c(2011, 2015, 2018, 2022), labels=c('DHS 2011', 'IMASIDA 2015', 'MIS 2018', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, .5)) + 
  labs(x = "",
       y = "",
       title = "Malaria prevalence according to RDT",
       subtitle = "Percentage of children age 6-59 months tested using a rapid diagnostic test (RDT) who are positive for malaria",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 42")


# Mozambique Provincial Trend
df_malaria_prev_moz <- df_malaria_prev %>% 
  filter(country == "Mozambique",
         characteristic != "Total")

df_malaria_prev_moz %>% 
  mutate(value = value / 100) %>% 
  ggplot(aes(year, value, color = characteristic)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_blank(),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = label_percent()(value)),
            data = df_malaria_prev_moz %>% mutate(value = value / 100) %>% filter(year == "2022"),
            size = 3,
            vjust = -.5, 
            hjust = 1) +
  scale_x_continuous(breaks=c(2011, 2015, 2018, 2022), labels=c('DHS 2011', 'IMASIDA 2015', 'MIS 2018', 'DHS 2022')) +
  scale_y_continuous(labels = percent) + 
  labs(x = "",
       y = "",
       title = "Malaria Prevalence by Province",
       subtitle = "Percentage of children age 6-59 months tested using a rapid diagnostic test (RDT) who are positive for malaria",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 42") +
  facet_wrap(~ factor(characteristic, levels=c('Niassa', 'Cabo Delgado', 'Nampula', 'Zambézia', 'Tete', 'Manica', 'Sofala', 'Inhambane', 'Gaza', 'Maputo Provincia', 'Maputo Cidade')))

# Mozambique Provincial Order
df_malaria_prev %>% 
  filter(survey == "2022 DHS",
         characteristic != "Total") %>% 
  mutate(value = value / 100) %>% 
  ggplot(aes(x = fct_reorder(characteristic, value), y = value, fill = characteristic)) +
  geom_col(width = 0.75) +
  scale_y_continuous(labels = percent) + 
  theme_fivethirtyeight() +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = percent(value, 1)), 
            hjust = 1.5, 
            colour = "white") +
  labs(x = "",
       y = "",
       title = "Malaria prevalence by Province (2022)",
       subtitle = "",
       caption = "Source: 2022 DHS Key Indicator Report, Page 42")


# MCH ANC Skilled Provider ------------------------------------------

df_mch_anc_provider <- load_clean_province_long("Antenatal by skilled health provider")

# Mozambique National Trend
df_mch_anc_provider %>% 
  filter(country == "Mozambique",
         characteristic == "Total") %>% 
  ggplot(aes(year, value)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = scales::percent(value, 1)),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2015, 2018, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'IMASIDA 2015', 'MIS 2018', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Antenatal care from a skilled provider",
       subtitle = "Percentage of women who had a live birth in the two years preceding the survey who received \nantenatal care during the pregnancy for the most recent live birth from a skilled provider",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 21")


# Mozambique Provincial Order
df_mch_anc_provider %>% 
  filter(survey == "2022 DHS",
         !characteristic %in% c("Total", "Urban", "Rural")) %>% 
  ggplot(aes(x = fct_reorder(characteristic, value), y = value, fill = characteristic)) +
  geom_col(width = 0.75) +
  scale_y_continuous(labels = percent) + 
  theme_fivethirtyeight() +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = percent(value, 1)), 
            hjust = 1.5, 
            colour = "white") +
  labs(x = "",
       y = "",
       title = "ANC by Skilled Provider by Province (2022)",
       subtitle = "",
       caption = "Source: 2022 DHS Key Indicator Report, Page 21")


# Mozambique Provincial Trend
df_mch_anc_provider_moz <- df_mch_anc_provider %>% 
  filter(country == "Mozambique",
         !characteristic %in% c("Total", "Urban", "Rural"))

df_mch_anc_provider_moz %>% 
  ggplot(aes(year, value, color = characteristic)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_blank(),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = percent(value, 1)), 
            data = df_mch_anc_provider_moz %>%  filter(year == "2022"),
            size = 3,
            vjust = -.5, 
            hjust = 1) +
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1.1)) + 
  labs(x = "",
       y = "",
       title = "Antenatal Care from a Skilled Provider by Province",
       subtitle = "Percentage of women who had a live birth in the two years preceding the survey who received antenatal care during the pregnancy for the most recent \nlive birth from a skilled provider",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 21") +
  facet_wrap(~ factor(characteristic, levels=c('Niassa', 'Cabo Delgado', 'Nampula', 'Zambézia', 'Tete', 'Manica', 'Sofala', 'Inhambane', 'Gaza', 'Maputo Provincia', 'Maputo Cidade')))



# MCH 4 ANC ------------------------------------------

df_mch_anc4 <- load_clean_province_long("Antenatal visits for pregnancy: 4+ visits")


# Mozambique National Trend
df_mch_anc4 %>% 
  filter(country == "Mozambique",
         characteristic == "Total") %>% 
  ggplot(aes(year, value)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = scales::percent(value, 1)),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2015, 2018, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'IMASIDA 2015', 'MIS 2018', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Antenatal visits for pregnancy: 4+ visits",
       subtitle = "Percentage of women who had a live birth in the two years preceding the survey who had 4+ antenatal care visits",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 21")


# Mozambique Provincial Order
df_mch_anc4 %>% 
  filter(survey == "2022 DHS",
         characteristic != "Total") %>% 
  ggplot(aes(x = fct_reorder(characteristic, value), y = value, fill = characteristic)) +
  geom_col(width = 0.75) +
  scale_y_continuous(labels = percent) + 
  theme_fivethirtyeight() +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = percent(value, 1)), 
            hjust = 1.5, 
            colour = "white") +
  labs(x = "",
       y = "",
       title = "Antenatal visits for pregnancy: 4+ visits by Province",
       subtitle = "",
       caption = "Source: 2022 DHS Key Indicator Report, Page 21")


# Mozambique Provincial Trend
df_mch_anc4_moz <- df_mch_anc4 %>% 
  filter(country == "Mozambique",
         characteristic != "Total")

df_mch_anc4_moz %>% 
  ggplot(aes(year, value, color = characteristic)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_blank(),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = percent(value, 1)), 
            data = df_mch_anc4_moz %>%  filter(year == "2022"),
            size = 3,
            vjust = -.5, 
            hjust = 1) +
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Antenatal visits for pregnancy: 4+ visits by Province",
       subtitle = "Percentage of women who had a live birth in the two years preceding the survey who had 4+ antenatal care visits",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 21") +
  facet_wrap(~ factor(characteristic, levels=c('Niassa', 'Cabo Delgado', 'Nampula', 'Zambézia', 'Tete', 'Manica', 'Sofala', 'Inhambane', 'Gaza', 'Maputo Provincia', 'Maputo Cidade')))



# MCH Assistance During Delivery ------------------------------------------

df_mch_delivery <- load_clean_province_long("Assistance during delivery from a skilled provider")


# Mozambique National Trend
df_mch_delivery %>% 
  filter(country == "Mozambique",
         characteristic == "Total") %>% 
  ggplot(aes(year, value)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = scales::percent(value, 1)),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Assistance during delivery from a skilled provider",
       subtitle = "Percentage of live births in the three years preceding the survey assisted by a skilled provider. \nSkilled provider includes doctor, nurse, midwife and auxiliary nurse or midwife.",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 21")


# Mozambique Provincial Order
df_mch_delivery %>% 
  filter(survey == "2022 DHS",
         characteristic != "Total") %>% 
  ggplot(aes(x = fct_reorder(characteristic, value), y = value, fill = characteristic)) +
  geom_col(width = 0.75) +
  scale_y_continuous(labels = percent) + 
  theme_fivethirtyeight() +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = percent(value, 1)), 
            hjust = 1.5, 
            colour = "white") +
  labs(x = "",
       y = "",
       title = "Assistance during delivery by Province",
       subtitle = "",
       caption = "Source: 2022 DHS Key Indicator Report, Page 21")


# Mozambique Provincial Trend
df_mch_delivery_moz <- df_mch_delivery %>% 
  filter(country == "Mozambique",
         characteristic != "Total")

df_mch_delivery_moz %>% 
  ggplot(aes(year, value, color = characteristic)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_blank(),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = percent(value, 1)), 
            data = df_mch_delivery_moz %>%  filter(year == "2022"),
            size = 3,
            vjust = -.5, 
            hjust = 1) +
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Assistance during delivery from a skilled provider",
       subtitle = "Percentage of live births in the three years preceding the survey assisted by a skilled provider. \nSkilled provider includes doctor, nurse, midwife and auxiliary nurse or midwife.",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 21") +
  facet_wrap(~ factor(characteristic, levels=c('Niassa', 'Cabo Delgado', 'Nampula', 'Zambézia', 'Tete', 'Manica', 'Sofala', 'Inhambane', 'Gaza', 'Maputo Provincia', 'Maputo Cidade')))


 
# Newborn First Postnatal Checkup ------------------------------------------

df_newborn_2days <- load_clean_province_long("Newborn's first postnatal checkup in the first two days after birth")


# Mozambique National Trend
df_newborn_2days %>% 
  filter(country == "Mozambique",
         characteristic %in% c("Total", "Rural", "Urban")) %>% 
  ggplot(aes(x = factor(characteristic, levels = c("Total", "Rural", "Urban")), value, fill = survey)) + 
  geom_col(position = "dodge") +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 10),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = scales::percent(value, 1)),
            size = 3.5,
            vjust = -.75,
            position = position_dodge(width = 1)) +
  scale_y_continuous(labels = percent,
                     limits = c(0, .6)) + 
  labs(x = "",
       y = "",
       title = "Newborn's 1st postnatal checkup in first 2 days",
       subtitle = "Percentage of last births in the two years preceding the survey who had their first postnatal \ncheckup within the first two days after birth",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 21")


# Mozambique Provincial Order
df_newborn_2days %>% 
  filter(survey == "2022 DHS",
         !characteristic %in% c("Total", "Rural", "Urban")) %>% 
  ggplot(aes(x = fct_reorder(characteristic, value), y = value, fill = characteristic)) +
  geom_col(width = 0.75) +
  scale_y_continuous(labels = percent) + 
  theme_fivethirtyeight() +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        legend.position= "none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = percent(value, 1)), 
            hjust = 1.5, 
            colour = "white") +
  labs(x = "",
       y = "",
       title = "Newborn's 1st postnatal checkup by Province",
       subtitle = "",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 21")


# Mozambique Provincial Trend
df_newborn_2days_moz <- df_newborn_2days %>% 
  filter(country == "Mozambique",
         !characteristic %in% c("Total", "Rural", "Urban")) 
  
df_newborn_2days_moz %>% 
  ggplot(aes(year, value, color = characteristic)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_blank(),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = percent(value, 1)), 
            data = df_newborn_2days_moz %>%  filter(year == "2022"),
            size = 3,
            vjust = -.5, 
            hjust = 1) +
  scale_x_continuous(breaks=c(2015, 2022), labels=c('IMASIDA 2015', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Newborn's 1st postnatal checkup by Province",
       subtitle = "Percentage of last births in the two years preceding the survey who had their first postnatal checkup within the first two days after birth",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 21") +
  facet_wrap(~ factor(characteristic, levels=c('Niassa', 'Cabo Delgado', 'Nampula', 'Zambézia', 'Tete', 'Manica', 'Sofala', 'Inhambane', 'Gaza', 'Maputo Provincia', 'Maputo Cidade')))





# RNMCH Summary -----------------------------------------------------------

df_rnmch_sum <- bind_rows(df_mch_delivery, df_mch_anc4, df_newborn_2days, df_mch_anc_provider) %>% 
  filter(characteristic == "Total") %>% 
  mutate(indicator = case_when(indicator == "Antenatal visits for pregnancy: 4+ visits" ~ "4 ANC",
                               indicator == "Assistance during delivery from a skilled provider" ~ "Provider at Delivery",
                               indicator == "Newborn's first postnatal checkup in the first two days after birth" ~ "Postnatal Checkup at 2 days",
                               indicator == "Antenatal by skilled health provider" ~ "ANC by skilled provider")
  )

df_rnmch_sum %>% 
  ggplot(aes(year, value, color = indicator)) + 
  geom_line(size = 1, alpha = .75) +
  geom_point(size = 1.5, alpha = .9) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = indicator),
            data = df_rnmch_sum %>% filter(year == "2022"),
            size = 3.5,
            vjust = -.75,
            hjust = 1,
            nudge_y = 0, 
            nudge_x = -2) +
  geom_text(aes(label = scales::percent(value, 1)),
            data = df_rnmch_sum %>% filter(year == "2022"),
            size = 3.5,
            vjust = -.75,
            hjust = 1,
            nudge_y = 0, 
            nudge_x = 0) +
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2015, 2018, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'IMASIDA 2015', 'MIS 2018', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Summary of RNMCH DHS Indicators",
       subtitle = "* Percentage of women who had a live birth in the two years preceding the survey who received antenatal care during the pregnancy\n for the most recent live birth from a skilled provider\n* Percentage of women who had a live birth in the two years preceding the survey who had 4+ antenatal care visits. \n* Percentage of live births in the three years preceding the survey assisted by a skilled provider. \n* Percentage of last births in the two years preceding the survey who had their first postnatal",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 21")


# Family Planning Summary -------------------------------------------------

df_fp_summary <- read_sheet(as_sheets_id(gs_id), 
                 sheet = "Family planning summary")  %>% 
  pivot_longer(`Current using modern methods`:`Unmet Need`, names_to = "indicator", values_to = "value") %>% 
  clean_names() %>% 
  mutate(characteristic = str_remove_all(characteristic, "Provinces : |L1|\\(|\\)"),
         characteristic = str_remove_all(characteristic, "Residence : |L1|\\(|\\)"),
         characteristic = str_trim(characteristic, side = "right"),
         characteristic = str_remove_all(characteristic, " 15-49"),
         value = value / 100) %>% 
  separate(survey, sep = 4, c("year", "temp"), remove = FALSE) %>% 
  mutate(year = as.numeric(year)) %>% 
  select(!temp) %>% 
  relocate(value, .after = everything())


df_fp_demand <- df_fp_summary %>% 
  filter(indicator == "Total Demand",
         characteristic == "Total")

df_fp_summary %>% 
  filter(indicator != "Total Demand",
         characteristic == "Total") %>% 
  ggplot(aes(year, value, fill = factor(indicator, levels = c('Unmet Need', 'Currently using traditional methods', 'Current using modern methods')))) +
  geom_col(position = "stack") +
  geom_point(aes(year, value),
             size = 0, alpha = 0,
             data = df_fp_demand) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2015, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'IMASIDA 2015', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, .6)) +
  geom_text(aes(label = scales::percent(value, 1)),
            colour = "white",
            size = 3.5,
            vjust = 1,
            hjust = .5,
            nudge_y = 0, 
            nudge_x = 0) +
  geom_text(aes(label = scales::percent(value, 1)),
            data = df_fp_demand,
            size = 3.5,
            vjust = -.5,
            hjust = .5,
            nudge_y = 0, 
            nudge_x = 0) +
  labs(x = "",
       y = "",
       title = "Trends in use, need, and demand for family planning",
       subtitle = "Percentage of currently married women aged 15-49",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 17")
  


# Family Planning Types ---------------------------------------------------

df_fp_type <- read_sheet(as_sheets_id(gs_id), 
                         sheet = "Family planning type",
                         col_types = c("ccnnnnnnnnnnnnnnnnnnnn"))  %>%
  pivot_longer(Sterilization:Other, names_to = "indicator", values_to = "value") %>% 
  clean_names() %>% 
  select(category, characteristic, indicator, value)

df_fp_type %>% 
  mutate(indicator = fct_lump(indicator, n = 7)) %>% 
  filter(category == "Province",
         !indicator %in% c('Emergency Contraception', 'Female Condom', 'Standard Day Method', 'Lactation Method')) %>% 
  ggplot(aes(x = factor(characteristic, levels=c('Niassa', 'Cabo Delgado', 'Nampula', 'Zambézia', 'Tete', 'Manica', 'Sofala', 'Inhambane', 'Gaza', 'Maputo Provincia', 'Maputo Cidade')), y = value, fill = indicator)) +
  geom_col(position = "fill") +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust=1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  scale_fill_discrete() +
  scale_y_continuous(labels = percent) +
  labs(x = "",
       y = "",
       title = "Current use of anticontraceptive methods by type",
       subtitle = "Percentage distribution among woment currently married or in union and among sexually active women age 15-49 by method type",
       caption = "2022 DHS Key Indicator Report, Page 15")


df_fp_type %>% 
  filter(category == "Wealth") %>% 
  ggplot(aes(x = factor(characteristic, levels=c('Lowest', 'Low', 'Middle', 'High', 'Highest')), y = value, fill = indicator)) +
  geom_col(position = "fill") +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  scale_y_continuous(labels = percent) +
  labs(x = "",
       y = "",
       title = "Current use of Contraception by Wealth Quintile",
       subtitle = "Percentage distribution among women currently married or in union and among sexually active women age \n15-49 by method type",
       caption = "")


df_fp_type %>% 
  filter(category == "Wealth") %>% 
  ggplot(aes(x = characteristic, value, fill = indicator)) +
  geom_col(position = "fill") +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  scale_y_continuous(labels = percent) +
  labs(x = "",
       y = "",
       title = "Current use of anticontraceptive methods by type",
       subtitle = "Percentage distribution among woment currently married or in union and among sexually active women age \n15-49 by method type",
       caption = "")


# Nutrition Women with Anemia ---------------------------------------------

df_nut_anemia_pw <- read_sheet(as_sheets_id(gs_id), 
                            sheet = "Women with Anemia")  %>% 
  pivot_longer(`Women with any anemia`:`Women with severe anemia`, names_to = "indicator", values_to = "value") %>% 
  clean_names() %>% 
  mutate(characteristic = str_remove_all(characteristic, "Provinces : |L1|\\(|\\)"),
         characteristic = str_remove_all(characteristic, "Residence : |L1|\\(|\\)"),
         characteristic = str_remove_all(characteristic, "Age grouped : "),
         characteristic = str_trim(characteristic, side = "right"),
         characteristic = str_remove_all(characteristic, " 15-49"),
         group = case_when(characteristic %in% c("Rural", "Urban") ~ "Residence",
                           characteristic %in% c("Total") ~ "Total",
                           str_detect(characteristic, "-") ~ "Age",
                           str_detect(characteristic, "Maternity status") ~ "Maternity status",
                           .default = "Province"),
         characteristic = str_remove_all(characteristic, "Maternity status : "),
         indicator = str_remove_all(indicator, "Women with "),
         indicator = str_to_title(indicator),
         value = value / 100) %>% 
  separate(survey, sep = 4, c("year", "temp"), remove = FALSE) %>% 
  mutate(year = as.numeric(year)) %>% 
  select(!temp) %>% 
  relocate(value, .after = everything()) %>% 
  relocate(group, .before = characteristic)


# Mozambique National Trend - Bar
df_nut_anemia_pw %>% 
  filter(country == "Mozambique",
         characteristic %in% c("Total")) %>% 
  ggplot(aes(x = factor(indicator, levels = c("Any Anemia", "Mild Anemia", "Moderate Anemia", "Severe Anemia")), value, fill = survey)) + 
  geom_col(position = "dodge") +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 10),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  scale_y_continuous(labels = percent,
                     limits = c(0, .6)) +
  geom_text(aes(label = scales::percent(value, 1)),
            size = 3.5,
            vjust = -.75,
            position = position_dodge(width = 1)) +
  labs(x = "",
       y = "",
       title = "Prevalence of Anemia in Women",
       subtitle = " * Percentage of women 15-49 classified as having any anemia (<12.0 g/dl for non-pregnant women and <11.0 g/dl for pregnant women)\n * Percentage of women 15-49 classified as having mild anemia (11.0-11.9 g/dl for non-pregnant women and 10.0-10.9 g/dl for pregnant women)\n * Percentage of women 15-49 classified as having moderate anemia (8.0-10.9 g/dl for non-pregnant women and 7.0-9.9 g/dl for pregnant women)\n * Percentage of women 15-49 classified as having severe anemia (<8.0 g/dl for non-pregnant women and <7.0 g/dl for pregnant women)",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 34")





# Mozambique Provincial Trend
df_nut_anemia_pw_moz <- df_nut_anemia_pw %>% 
  filter(country == "Mozambique",
         group == "Province",
         indicator != "Mild Anemia")

df_nut_anemia_pw_moz %>% 
  ggplot(aes(year, value, color = indicator)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = percent(value, 1)), 
            data = df_nut_anemia_pw_moz %>%  filter(year == "2022"),
            size = 3,
            vjust = -.5, 
            hjust = 1) +
  geom_text(aes(label = percent(value, 1)), 
            data = df_nut_anemia_pw_moz %>%  filter(year == "2011"),
            size = 3,
            vjust = -.5, 
            hjust = -0) +
  scale_x_continuous(breaks=c(2011, 2022), labels=c('DHS 2011', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Prevalence of Anemia in Women by Province",
       subtitle = " * Percentage of women 15-49 classified as having any anemia (<12.0 g/dl for non-pregnant women and <11.0 g/dl for pregnant women)\n * Percentage of women 15-49 classified as having mild anemia (11.0-11.9 g/dl for non-pregnant women and 10.0-10.9 g/dl for pregnant women)\n * Percentage of women 15-49 classified as having moderate anemia (8.0-10.9 g/dl for non-pregnant women and 7.0-9.9 g/dl for pregnant women)\n * Percentage of women 15-49 classified as having severe anemia (<8.0 g/dl for non-pregnant women and <7.0 g/dl for pregnant women)",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 34") +
  facet_wrap(~ factor(characteristic, levels=c('Niassa', 'Cabo Delgado', 'Nampula', 'Zambézia', 'Tete', 'Manica', 'Sofala', 'Inhambane', 'Gaza', 'Maputo Provincia', 'Maputo Cidade')))



# Nutrition Children with Anemia ---------------------------------------------

df_nut_anemia_child <- read_sheet(as_sheets_id(gs_id), 
                               sheet = "Children with Anemia")  %>% 
  pivot_longer(`Children with any anemia`:`Children with severe anemia`, names_to = "indicator", values_to = "value") %>% 
  clean_names() %>% 
  mutate(characteristic = str_remove_all(characteristic, "Provinces : |L1|\\(|\\)"),
         characteristic = str_remove_all(characteristic, "Residence : |L1|\\(|\\)"),
         characteristic = str_remove_all(characteristic, "Age grouped : "),
         characteristic = str_trim(characteristic, side = "right"),
         characteristic = str_remove_all(characteristic, " 15-49"),
         group = case_when(characteristic %in% c("Rural", "Urban") ~ "Residence",
                           characteristic %in% c("Total") ~ "Total",
                           str_detect(characteristic, "-") ~ "Age",
                           str_detect(characteristic, "Maternity status") ~ "Maternity status",
                           .default = "Province"),
         characteristic = str_remove_all(characteristic, "Maternity status : "),
         value = value / 100) %>% 
  separate(survey, sep = 4, c("year", "temp"), remove = FALSE) %>% 
  mutate(year = as.numeric(year)) %>% 
  select(!temp) %>% 
  relocate(value, .after = everything()) %>% 
  relocate(group, .before = characteristic) %>% 
  mutate(indicator = str_remove_all(indicator, "Children with "),
         indicator = str_to_title(indicator))


# Mozambique National Trend - Line by Type
df_nut_anemia_child %>% 
  filter(country == "Mozambique",
         characteristic == "Total") %>% 
  ggplot(aes(year, value, color = indicator)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = scales::percent(value, 1)),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks=c(2011, 2015, 2018, 2022), labels=c('DHS 2011', 'IMASIDA 2015', 'MIS 2018', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Children with Anemia",
       subtitle = "* 	Percentage of children under age 5 classified as having any anemia\n* Percentage of children under age 5 classified as having mild (10.0-10.9 g/dl) anemia\n* Percentage of children under age 5 classified as having moderate (7.0-9.9 g/dl) anemia\n*	 Percentage of children under age 5 classified as having severe (below 7.0 g/dl) anemia",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 39")


# Mozambique National Trend - Bar by Severity
df_nut_anemia_child %>% 
  filter(country == "Mozambique",
         characteristic == "Total",
         indicator != "Any Anemia") %>% 
  ggplot(aes(year, value, fill = indicator)) + 
  geom_bar(alpha = .75, position = "fill", stat = "identity") + 
  scale_fill_manual(values=c("#7CAE00", "#00BFC4", "#C77CFF")) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8.5, angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=c(2011, 2015, 2018, 2022), labels=c('DHS 2011', 'IMASIDA 2015', 'MIS 2018', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Children with Anemia by Severity",
       subtitle = "* Percentage of children under age 5 classified as having mild (10.0-10.9 g/dl) anemia\n* Percentage of children under age 5 classified as having moderate (7.0-9.9 g/dl) anemia\n*	 Percentage of children under age 5 classified as having severe (below 7.0 g/dl) anemia",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 39")


# Nutrition Children Stunted ---------------------------------------------

df_nut_stunted <- load_clean_province_long("Children stunted")


# Mozambique National Trend
df_nut_stunted %>% 
  filter(country == "Mozambique",
         characteristic == "Total") %>% 
  ggplot(aes(year, value)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = scales::percent(value, 1)),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2019, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'IOF 2019', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, .8)) + 
  labs(x = "",
       y = "",
       title = "Child Stunting",
       subtitle = "* Percentage of children stunted (below -2 SD of height for age according to the WHO standard)",
       caption = "Sources:\nStatcompiler, 2019 IOF Report, Page 19, 2022 DHS Key Indicator Report, Page 28")



# Mozambique Provincial Order
df_nut_stunted %>% 
  filter(survey == "2022 DHS",
         characteristic != "Total") %>% 
  ggplot(aes(x = fct_reorder(characteristic, value), y = value, fill = characteristic)) +
  geom_col(width = 0.75) +
  scale_y_continuous(labels = percent) + 
  theme_fivethirtyeight() +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = percent(value, 1)), 
            hjust = 1.5, 
            colour = "white") +
  labs(x = "",
       y = "",
       title = "Child Stunting (2022)",
       subtitle = "",
       caption = "Sources: 2022 DHS Key Indicator Report, Page 28")


# Horizontal Bar by province
df_nut_stunted %>% 
  filter(survey == "2022 DHS",
         characteristic != "Total",
         !str_detect(characteristic, "Wealth quintile")) %>% 
  ggplot(aes(x = fct_reorder(characteristic, value, .desc = TRUE), y = value)) +
  geom_col(width = 0.75, fill = "#F8766D") +
  scale_y_continuous(labels = percent) + 
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 16, vjust = 0),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust=1),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  scale_y_continuous(labels = percent,
                     limits = c(0, .5)) + 
  geom_text(aes(label = percent(value, 1)), 
            size = 3.5,
            vjust = -1,
            hjust = .5, 
            colour = "#F8766D") +
  labs(x = "",
       y = "",
       title = "Child Stunting by Province (2022)",
       subtitle = "")


# Horizontal Bar by wealth quintile
df_nut_stunted %>% 
  filter(survey == "2022 DHS",
         characteristic != "Total",
         str_detect(characteristic, "Wealth quintile")) %>% 
  ggplot(aes(x = fct_reorder(characteristic, value, .desc = FALSE), y = value)) +
  geom_col(width = 0.75, fill = "#F8766D") +
  scale_y_continuous(labels = percent) + 
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 16, vjust = 0),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        axis.text.x = element_text(size = 9, vjust = 1, hjust=1),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  scale_y_continuous(labels = percent,
                     limits = c(0, .5)) + 
  geom_text(aes(label = percent(value, 1)), 
            size = 3.5,
            vjust = 0,
            hjust = -.25, 
            colour = "#F8766D") +
  coord_flip() +
  labs(x = "",
       y = "",
       title = "Child Stunting by Wealth Quintile (2022)",
       caption = "Sources: 2022 DHS Key Indicator Report, Page 28")


# Mozambique Provincial Trend
df_nut_stunted_moz <- df_nut_stunted %>% 
  filter(country == "Mozambique",
         characteristic != "Total")

df_nut_stunted_moz %>% 
  ggplot(aes(year, value, color = characteristic)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_blank(),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = percent(value, 1)), 
            data = df_nut_stunted_moz %>%  filter(year == "2022"),
            size = 3,
            vjust = -.5, 
            hjust = 1) +
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Child Stunting by Province",
       subtitle = "* Percentage of children stunted (below -2 SD of height for age according to the WHO standard)",
       caption = "Sources:\nStatcompiler, 2019 IOF Report, Page 19, 2022 DHS Key Indicator Report, Page 28") +
facet_wrap(~ factor(characteristic, levels=c('Niassa', 'Cabo Delgado', 'Nampula', 'Zambézia', 'Tete', 'Manica', 'Sofala', 'Inhambane', 'Gaza', 'Maputo Provincia', 'Maputo Cidade')))




# Nutrition Children Wasted ---------------------------------------------

df_nut_wasted <- load_clean_province_long("Children wasted")


# Mozambique National Trend
df_nut_wasted %>% 
  filter(country == "Mozambique",
         characteristic == "Total") %>% 
  ggplot(aes(year, value)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = scales::percent(value)),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2019, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'IOF 2019', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, .2)) + 
  labs(x = "",
       y = "",
       title = "Child Wasting",
       subtitle = "* Percentage of children wasted (below -2 SD of weight for height according to the WHO standard)",
       caption = "Sources:\nStatcompiler, 2019 IOF Report, Page 20, 2022 DHS Key Indicator Report, Page 28")



# Mozambique Provincial Order
df_nut_wasted %>% 
  filter(survey == "2022 DHS",
         characteristic != "Total") %>% 
  ggplot(aes(x = fct_reorder(characteristic, value), y = value, fill = characteristic)) +
  geom_col(width = 0.75) +
  scale_y_continuous(labels = percent) + 
  theme_fivethirtyeight() +
  si_style_xgrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  coord_flip() +
  geom_text(aes(label = percent(value, 1)), 
            hjust = 1.5, 
            colour = "white") +
  labs(x = "",
       y = "",
       title = "Child Wasting (2022)",
       subtitle = "",
       caption = "Sources: 2022 DHS Key Indicator Report, Page 28")


# Horizontal Bar
df_nut_wasted %>% 
  filter(survey == "2022 DHS",
         characteristic != "Total") %>% 
  ggplot(aes(x = fct_reorder(characteristic, value, .desc = TRUE), y = value)) +
  geom_col(width = 0.75, fill = "#00BFC4") +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        panel.spacing = unit(.75, "cm"),
        plot.title = element_text(size = 16, vjust = 0),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9),
        axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust=1),
        legend.position="none",
        legend.direction = "vertical",
        legend.title = element_blank()) + 
  scale_y_continuous(labels = percent,
                     limits = c(0, .1)) + 
  geom_text(aes(label = percent(value)), 
            size = 3.5,
            vjust = -1,
            hjust = .5, 
            colour = "#00BFC4") +
  labs(x = "",
       y = "",
       title = "Child Wasting by Province (2022)",
       subtitle = "",
       caption = "Sources: 2022 DHS Key Indicator Report, Page 28")


# Mozambique Provincial Trend
df_nut_wasted_moz <- df_nut_wasted %>% 
  filter(country == "Mozambique",
         characteristic != "Total")

df_nut_wasted_moz %>% 
  ggplot(aes(year, value, color = characteristic)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_blank(),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = percent(value, 1)), 
            data = df_nut_stunted_moz %>%  filter(year == "2022"),
            size = 3,
            vjust = -.5, 
            hjust = 1) +
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Child Wasting by Province",
       subtitle = "* Percentage of children stunted (below -2 SD of height for age according to the WHO standard)",
       caption = "Sources:\nStatcompiler, 2019 IOF Report, Page 19, 2022 DHS Key Indicator Report, Page 28") +
  facet_wrap(~ factor(characteristic, levels=c('Niassa', 'Cabo Delgado', 'Nampula', 'Zambézia', 'Tete', 'Manica', 'Sofala', 'Inhambane', 'Gaza', 'Maputo Provincia', 'Maputo Cidade')))



# Nutrition Children Stunted/Wasted ---------------------------------------------

df_nut_sum <- bind_rows(df_nut_stunted, df_nut_wasted) %>% 
  filter(characteristic == "Total")

df_nut_sum_prov <- bind_rows(df_nut_stunted, df_nut_wasted) %>% 
  filter(characteristic != "Total")

df_nut_sum %>% 
  ggplot(aes(year, value, color = indicator)) + 
  geom_line(size = 1, alpha = .75) +
  geom_point(size = 1.5, alpha = .9) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = indicator),
            data = df_nut_sum %>% filter(year == "2022"),
            size = 3.5,
            vjust = -3.5,
            hjust = .6,
            nudge_y = 0, 
            nudge_x = -2) +
  geom_text(aes(label = scales::percent(value, 1)),
            data = df_nut_sum %>% filter(indicator == "Children stunted"),
            size = 3.5,
            vjust = -1,
            hjust = 0,
            nudge_y = 0, 
            nudge_x = -1) +
  geom_text(aes(label = scales::percent(value)),
            data = df_nut_sum %>% filter(indicator == "Children wasted"),
            size = 3.5,
            vjust = -1,
            hjust = 0,
            nudge_y = 0, 
            nudge_x = -1) +
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2019, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'IOF 2019', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, .5)) + 
  labs(x = "",
       y = "",
       title = "Child Stunting and Wasting",
       subtitle = "* Percentage of children stunted (below -2 SD of height for age according to the WHO standard)\n* Percentage of children wasted (below -2 SD of weight for height according to the WHO standard)",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 21")



df_nut_sum_prov %>% 
  ggplot(aes(year, value, color = indicator)) + 
  geom_line(size = 1, alpha = .75) +
  geom_point(size = 1.5, alpha = .9) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust=1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=c(1997, 2003, 2011, 2019, 2022), labels=c('DHS 1997', 'DHS 2003', 'DHS 2011', 'IOF 2019', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, .75)) + 
  labs(x = "",
       y = "",
       title = "Child Stunting and Wasting",
       subtitle = "* Percentage of children stunted (below -2 SD of height for age according to the WHO standard)\n* Percentage of children wasted (below -2 SD of weight for height according to the WHO standard)",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 21") + 
  facet_wrap(~ factor(characteristic, levels = c('Niassa', 'Cabo Delgado', 'Nampula', 'Zambézia', 'Tete', 'Manica', 'Sofala', 'Inhambane', 'Gaza', 'Maputo Provincia', 'Maputo Cidade')))



# HIV Testing History -----------------------------------------------------

df_hiv_testing <- read_sheet(as_sheets_id(gs_id), 
                             sheet = "HIV testing") %>% 
  pivot_longer(4:11, names_to = "indicator", values_to = "value") %>% 
  clean_names() %>% 
  mutate(characteristic = str_remove_all(characteristic, "Provinces : |L1|\\(|\\)"),
         characteristic = str_remove_all(characteristic, "Residence : |L1|\\(|\\)"),
         characteristic = str_trim(characteristic, side = "right"),
         characteristic = str_remove_all(characteristic, " 15-49"),
         value = value / 100) %>% 
  separate(survey, sep = 4, c("year", "temp"), remove = FALSE) %>% 
  mutate(year = as.numeric(year),
         sex = case_when(str_detect(indicator, "Women") ~ "Female",
                         str_detect(indicator, "Men") ~ "Male"),
         indicator = case_when(str_detect(indicator, "months") ~ "Receiving an HIV test and receiving test results in the last 12 months",
                               str_detect(indicator, "never") ~ "Never tested for HIV",
                               str_detect(indicator, "ever tested") ~ "Ever tested for HIV and received test results",
                               str_detect(indicator, "ever receiving") ~ "Ever receiving an HIV test")) %>% 
  select(!temp) %>% 
  relocate(value, .after = everything())

# Mozambique National Trend Never Tested
df_hiv_testing %>% 
  filter(country == "Mozambique",
         characteristic == "Total",
         indicator %in% c("Never tested for HIV")) %>% 
  ggplot(aes(year, value, color = sex)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = c(.70, .75),
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = scales::percent(value, 1)),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks=c(2003, 2009, 2011, 2015, 2022), labels=c('DHS 2003', 'INSIDA 2009', 'DHS 2011', 'IMASIDA 2015', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Never having tested for HIV",
       subtitle = "* 	Percentage of women/men who were never tested for HIV",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 48, 49")


# Mozambique National Trend Tested in preceding 12 months
df_hiv_testing %>% 
  filter(country == "Mozambique",
         characteristic == "Total",
         indicator %in% c("Receiving an HIV test and receiving test results in the last 12 months")) %>% 
  ggplot(aes(year, value, color = sex)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = c(.70, .75),
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = scales::percent(value, 1)),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks=c(2003, 2009, 2011, 2015, 2022), labels=c('DHS 2003', 'INSIDA 2009', 'DHS 2011', 'IMASIDA 2015', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Tested for HIV in preceeding 12 months",
       subtitle = "* Percentage of women/men who received an HIV test in the 12 months preceding the interview and received the test results",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 48, 49")


# Mozambique National Trend Tested in preceding 12 months
df_hiv_testing %>% 
  filter(country == "Mozambique",
         characteristic == "Total",
         indicator %in% c("Ever tested for HIV and received test results")) %>% 
  ggplot(aes(year, value, color = sex)) + 
  geom_line(size = 1, alpha = .75) +
  si_style_ygrid() +
  theme(plot.background = element_rect(fill = "#e7e7e5", colour = "#e7e7e5"),
        plot.title = element_text(size = 16, vjust = 4),
        plot.subtitle = element_text(face = "italic", size = 8, vjust = 6, color = "grey50"),
        plot.caption = element_text(size = 9, vjust = 1),
        panel.spacing = unit(.75, "cm"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1),
        legend.position = c(.70, .75),
        legend.direction = "vertical",
        legend.title = element_blank()) +
  geom_text(aes(label = scales::percent(value, 1)),
            size = 3.5,
            vjust = -.75, 
            hjust = .5) +
  scale_x_continuous(breaks=c(2003, 2009, 2011, 2015, 2022), labels=c('DHS 2003', 'INSIDA 2009', 'DHS 2011', 'IMASIDA 2015', 'DHS 2022')) +
  scale_y_continuous(labels = percent,
                     limits = c(0, 1)) + 
  labs(x = "",
       y = "",
       title = "Ever tested for HIV and received results",
       subtitle = "*	Percentage of women/men who have ever had an HIV test and received their results",
       caption = "Sources: Statcompiler & 2022 DHS Key Indicator Report, Page 48, 49")
