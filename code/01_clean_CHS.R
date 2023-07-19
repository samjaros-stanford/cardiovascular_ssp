library(haven)
library(tidyverse)

##########
# Import #
##########

# Baseline data from Hoda
#   Already 1 patient/line
#   ID = idno
raw_chs = read.csv("raw_data/AbdelMagid_CHS_Data092822.csv")
raw_chs_tract = read_sas("raw_data/chs_census_baseline_2010.sas7bdat")
raw_chs_zcta = read_sas("raw_data/chs_zcta_baseline_2010.sas7bdat")

# Find missingness of tracts & ZCTAs
chs_geo_missingness = raw_chs %>%
  select(idno) %>%
  mutate(inMain = T) %>%
  full_join(raw_chs_tract %>% select(idno) %>% mutate(inTract=T), by="idno") %>%
  full_join(raw_chs_zcta %>% select(idno) %>% mutate(inZCTA=T), by="idno")
table(!is.na(chs_geo_missingness$inMain), !is.na(chs_geo_missingness$inTract))
table(!is.na(chs_geo_missingness$inMain), !is.na(chs_geo_missingness$inZCTA))
table(!is.na(chs_geo_missingness$inTract), !is.na(chs_geo_missingness$inZCTA))

##############
# Processing #
##############
# Get tracts & ZCTAs needed for ICE calcs
chs_geos = raw_chs_tract %>%
  select(idno, t10_cen_uid_u_2010) %>%
  rename(tract_FIPS11=t10_cen_uid_u_2010) %>%
  full_join(raw_chs_zcta %>%
              select(idno, z10_cen_uid_u_2010) %>%
              rename(zcta=z10_cen_uid_u_2010),
            by="idno")

##########
# Export #
##########
saveRDS(chs_geos, "data/chs_geos.rds")
