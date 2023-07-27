library(tidyverse)

##########
# Import #
##########
# ACS Data for REGARDS
ice_county_acs = read.csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_acs_2010_county.csv") %>%
  select(-ICEedu) %>%
  left_join(read.csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_acs_2012_county.csv") %>%
              select(GEOID, ICEedu),
            by="GEOID")
ice_zcta_acs = read.csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_acs_2011_zcta.csv") %>%
  select(-ICEedu) %>%
  left_join(read.csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_acs_2012_zcta.csv") %>%
              select(GEOID, ICEedu),
            by="GEOID")
ice_tract_acs = read.csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_acs_2010_tract.csv") %>%
  select(-ICEedu) %>%
  left_join(read.csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_acs_2012_tract.csv") %>%
              select(GEOID, ICEedu),
            by="GEOID")

# Decennial census data for CHS
ice_county_dec = read.csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_dec_2000_county.csv")
ice_zcta_dec = read.csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_dec_2000_zcta.csv")
ice_tract_dec = read.csv("https://raw.githubusercontent.com/samjaros-stanford/spatial_social_polarization_database/main/ICE/ice_dec_2000_tract.csv")

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

