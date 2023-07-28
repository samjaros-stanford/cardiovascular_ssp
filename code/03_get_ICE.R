library(tidyverse)

##########
# Import #
##########
# ACS Data for REGARDS
ice_county_acs = read_csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_acs_2010_county.csv", show_col_types=F) %>%
  select(-ICEedu) %>%
  left_join(read_csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_acs_2012_county.csv", show_col_types=F) %>%
              select(GEOID, ICEedu),
            by="GEOID") %>%
  rename_with(~paste0(.x,"_county"), -GEOID)
ice_zcta_acs = read_csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_acs_2011_zcta.csv", show_col_types=F) %>%
  select(-ICEedu) %>%
  left_join(read_csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_acs_2012_zcta.csv", show_col_types=F) %>%
              select(GEOID, ICEedu),
            by="GEOID") %>%
  rename_with(~paste0(.x,"_zcta"), -GEOID)
ice_tract_acs = read_csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_acs_2010_tract.csv", show_col_types=F) %>%
  select(-ICEedu) %>%
  left_join(read_csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_acs_2012_tract.csv", show_col_types=F) %>%
              select(GEOID, ICEedu),
            by="GEOID") %>%
  rename_with(~paste0(.x,"_tract"), -GEOID)

# Decennial census data for CHS
ice_county_dec = read_csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_dec_2000_county.csv", show_col_types=F) %>%
  rename_with(~paste0(.x,"_county"),-GEOID)
ice_zcta_dec = read_csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_dec_2000_zcta.csv", show_col_types=F) %>%
  rename_with(~paste0(.x,"_zcta"),-GEOID)
ice_tract_dec = read_csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_dec_2000_tract.csv", show_col_types=F) %>%
  rename_with(~paste0(.x,"_tract"),-GEOID)

##########
# Export #
##########
# ACS
saveRDS(ice_county_acs, "data/ICE/ice_county_acs.rds")
saveRDS(ice_zcta_acs, "data/ICE/ice_zcta_acs.rds")
saveRDS(ice_tract_acs, "data/ICE/ice_tract_acs.rds")
# Decennial
saveRDS(ice_county_dec, "data/ICE/ice_county_dec.rds")
saveRDS(ice_zcta_dec, "data/ICE/ice_zcta_dec.rds")
saveRDS(ice_tract_dec, "data/ICE/ice_tract_dec.rds")

