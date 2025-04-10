# spatial coverage
library(rworldmap)

# Remove rows where ISO_A2 is -99
iso2_df <- data.frame(countriesLow[countriesLow$ISO_A2 != "-99", c("ISO_A2", "NAME")])

# Rename columns for clarity
colnames(iso2_df) <- c("iso2", "country")

write.csv(iso2_df, "iso_2_country_name.csv")

iso_2_country_names = read.csv("iso_2_country_name.csv") %>% dplyr::select(-X)
usethis::use_data(iso_2_country_names, overwrite = TRUE)


# document("../occAssess")
# load_all("../occAssess")

library(devtools)

document()
load_all()
run_app()
