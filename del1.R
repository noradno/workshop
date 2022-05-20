# Script to load Norwegian development aid statistics into R

# Load packages and data --------------------------------------------------

library(noradstats)
library(tidyverse)
library(janitor)
library(writexl)

# Read aid data into R environment
df <- noradstats::read_aiddata("statsys_ten.csv")

df <- noradstats::add_cols_basic(df)

df <- janitor::clean_names(df)

glimpse(df)


# Top ten countries 2021 --------------------------------------------------

df_countries <- df |> 
  filter(type_of_flow == "ODA") |> 
  filter(type_of_agreement != "Rammeavtale") |> 
  filter(year == max(year)) |> 
  filter(income_category != "Unspecified") |> 
  group_by(recipient_country_no) |> 
  summarise(nok_mill = sum(disbursed_mill_nok)) |> 
  slice_max(nok_mill, n = 10)

df_partner <- df |> 
  filter(type_of_flow == "ODA") |> 
  filter(type_of_agreement != "Rammeavtale") |> 
  filter(year == max(year)) |> 
  group_by(partner_group_visual_no) |> 
  summarise(nok_mill = sum(disbursed_mill_nok)) |>
  arrange(desc(nok_mill))

# Figur

df_countries |> 
  ggplot(aes(x = fct_reorder(recipient_country_no, nok_mill), y = nok_mill)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = nok_mill), hjust = 0) +
  labs(
    title = paste("Største mottakerland i", max(df$year)),
    subtitle = "Beløp i millioner kroner",
    x = NULL,
    y = "Millioner kroner"
  ) +
  theme_minimal()
  


# Save to excel file ------------------------------------------------------

writexl::write_xlsx(df_countries, "del1.xlsx")




