# Script to load Norwegian development aid statistics into R

# Load packages and data --------------------------------------------------

library(noradstats)
library(tidyverse)
library(janitor)
library(writexl)

# Read aid data into R environment
df <- noradstats::read_aiddata("statsys_ten.csv")

glimpse(df)

df <- noradstats::add_cols_basic(df) |> 
  janitor::clean_names()


# Top ten countries 2021 --------------------------------------------------

df_countries <- df |> 
  filter(type_of_flow == "ODA") |> 
  filter(type_of_agreement != "Rammeavtale") |> 
  filter(year == max(year)) |> 
  filter(income_category != "Unspecified") |> 
  group_by(recipient_country_no) |> 
  summarise(nok_mill = sum(disbursed_mill_nok)) |> 
  slice_max(nok_mill, n = 10)


# Save to excel file ------------------------------------------------------

writexl::write_xlsx(df_countries, "del1.xlsx")




