# Script for SRHR

# Load packages and data --------------------------------------------------

# Load packages
library(noradstats)
library(tidyverse)
library(janitor)
library(writexl)

# Read aid data into R environment
df <- noradstats::read_aiddata("statsys_ten.csv")

# Clean column names
df <- noradstats::add_cols_basic(df) |> 
  janitor::clean_names()


# SRHR  --------------------------------------------------

#unique(df$dac_main_sector_code_name)

#str_subset(df$agreement_title, regex("global financing facility", ignore_case = TRUE))

df_new <- df |> 
  
  mutate(srhr = case_when(
    dac_main_sector_code_name == "130 - Population policies/programmes and reproductive health" ~ "130 - Population policies/programmes and reproductive health",
    type_of_assistance == "Core contributions to multilat" & agreement_partner == "UNAIDS - UN Programme on HIV/AIDS" ~ "Kjernestøtte til UNAIDS (100 pst)",
    type_of_assistance == "Core contributions to multilat" & agreement_partner == "UNFPA - UN Population Fund" ~ "Kjernestøtte til UNFPA (100 pst)",
    type_of_assistance == "Core contributions to multilat" & agreement_partner == "GFATM - Global Fund to Fight AIDS, Tuberculosis and Malaria" ~ "Kjernestøtte til GFATM (50 pst)",
    agreement_partner == "World Bank" & agreement_number %in% c("QZA-20/0303-1", "QZA-15/0421") ~ "Støtte til utvalgte avtaler med GFF (28 pst)",
    TRUE ~ "Annet"
    )) |> 
  
  mutate(srhr_nok = case_when(
    !srhr %in% c("Annet", "Kjernestøtte til GFATM (50 pst)") ~ disbursed_nok,
    srhr == "Kjernestøtte til GFATM (50 pst)" ~ disbursed_nok * 0.5,
    srhr == "Støtte til utvalgte avtaler med GFF (28 pst)" ~ disbursed_nok * 0.28
  ))

df_srhr <- df_new |> 
  filter(year %in% c(2017:2021)) |> 
  filter(srhr != "Annet") |> 
  group_by(srhr, year) |> 
  summarise(nok_mill = sum(srhr_nok) / 1e6) |> 
  ungroup()

df_srhr |> 
  pivot_wider(names_from = year, values_from = nok_mill) |> 
  adorn_totals("row")
