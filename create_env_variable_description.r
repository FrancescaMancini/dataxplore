library(dplyr)
library(tidyr)
library(stringr)

# 1. Create BIO lookup table
bio_table <- tribble(
  ~BIO, ~description,
  "BIO1",  "Annual Mean Temperature",
  "BIO2",  "Mean Diurnal Range (Mean of monthly (max temp - min temp))",
  "BIO3",  "Isothermality (BIO2/BIO7) (×100)",
  "BIO4",  "Temperature Seasonality (standard deviation ×100)",
  "BIO5",  "Max Temperature of Warmest Month",
  "BIO6",  "Min Temperature of Coldest Month",
  "BIO7",  "Temperature Annual Range (BIO5-BIO6)",
  "BIO8",  "Mean Temperature of Wettest Quarter",
  "BIO9",  "Mean Temperature of Driest Quarter",
  "BIO10", "Mean Temperature of Warmest Quarter",
  "BIO11", "Mean Temperature of Coldest Quarter",
  "BIO12", "Annual Precipitation",
  "BIO13", "Precipitation of Wettest Month",
  "BIO14", "Precipitation of Driest Month",
  "BIO15", "Precipitation Seasonality (Coefficient of Variation)",
  "BIO16", "Precipitation of Wettest Quarter",
  "BIO17", "Precipitation of Driest Quarter",
  "BIO18", "Precipitation of Warmest Quarter",
  "BIO19", "Precipitation of Coldest Quarter"
)

# 2. Convert aux_file to long format
aux_long <- aux_file %>%
  pivot_longer(cols = -monad, names_to = "auxcolumn", values_to = "value")

# 3. Extract BIO code from column names (if present)
aux_long <- aux_long %>%
  mutate(
    BIO = str_extract(auxcolumn, "bio_\\d+"),
    BIO = toupper(str_replace(BIO, "bio_", "BIO"))
  )

# 4. Merge with bio_table
aux_long <- aux_long %>%
  left_join(bio_table, by = "BIO") %>%
  mutate(
    description = ifelse(is.na(description), auxcolumn, description)
  ) %>%
  select(monad, auxcolumn, value, description)

aux_file = aux_long

library(usethis)

use_data(aux_file, overwrite = TRUE)

document()

load_all()