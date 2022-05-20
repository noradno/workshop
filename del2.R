# Script to load Norwegian development aid statistics and imputed multilateral ODA into R

# Load packages and data --------------------------------------------------

# Load packages
library(noradstats)
library(tidyverse)
library(janitor)
library(writexl)

# Read aid data into R environment
df <- noradstats::read_aiddata("statsys_ten.csv")

df <- noradstats::add_cols_basic(df) |> 
  janitor::clean_names()


# Earmarked to countries --------------------------------------------------

df_countries <- df |> 
  filter(type_of_flow == "ODA") |> 
  filter(type_of_agreement != "Rammeavtale") |> 
  filter(year == 2019) |> 
  filter(income_category != "Unspecified") |> 
  group_by(recipient_country_crs, recipient_country_no) |> 
  summarise(earmarked_nok_mill = sum(disbursed_mill_nok)) |> 
  filter(earmarked_nok_mill != 0) |> 
  ungroup()


# Imputed multilateral ODA to countries -----------------------------------

df_imputed <- noradstats::get_imputed(startyear = 2018, endyear = 2018)

glimpse(df_imputed)

df_imputed <- df_imputed |> 
  select(recipient, recipient_label_en, nok_mill) |> 
  mutate(recipient = as.numeric(recipient)) |> 
  rename(imputed_nok_mill = nok_mill)

# Remove totals
df_imputed <- df_imputed %>%
  filter(!str_detect(recipient_label_en, "Total"))

# Merge datasets of earmarked and imputed ---------------------------------

df_total <- full_join(df_countries, df_imputed, by = c("recipient_country_crs" = "recipient"))

df_total <- df_total |> 
  select(recipient_country_crs, recipient_country_no, recipient_label_en, earmarked_nok_mill, imputed_nok_mill) |> 
  mutate(recipient_country_crs = as.character(recipient_country_crs)) |> 
  adorn_totals("col") |> 
  arrange(desc(Total))

# Save table in excel file ------------------------------------------------------

writexl::write_xlsx(list("2020" = df_total2020, "2019" = df_total2019, "2018" = df_total2018), "land.xlsx")

df_total2018 <- df_total


