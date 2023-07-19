library(tidyverse)

##########
# Import #
##########
# Baseline data from Hoda
#   Already 1 person/line
#   ID = id
raw_regards= read.csv("raw_data/REGARDS_BASELINE_DATASET.csv")

# Find missingness in tracts and ZCTAs
sum(!grepl("[0-9]{5}", raw_regards$GEO_Zip, perl=T))/nrow(raw_regards)
sum(is.na(raw_regards$GEO_StateFIPS))/nrow(raw_regards)
sum(is.na(raw_regards$GEO_CountyFIPS))/nrow(raw_regards)
sum(is.na(raw_regards$GEO_Tract))/nrow(raw_regards)
table(is.na(raw_regards$GEO_CountyFIPS), is.na(raw_regards$GEO_Tract))

##############
# Processing #
##############
# Get tracts & ZCTAs needed for ICE calcs
#   Only keep valid ZCTAs
#   Only keep valid 11-digit FIPS codes
regards_geos = raw_regards %>%
  select(id, GEO_Zip, GEO_StateFIPS, GEO_CountyFIPS, GEO_Tract) %>%
  mutate(zcta = if_else(grepl("[0-9]{5}", GEO_Zip, perl=T), GEO_Zip, NA_character_),
         state_FIPS = str_pad(GEO_StateFIPS,2,"left","0"), 
         county_FIPS = str_pad(GEO_CountyFIPS,3,"left","0"), 
         tract_FIPS = str_pad(GEO_Tract,6,"left","0"),
         tract_FIPS11 = str_c(state_FIPS, county_FIPS, tract_FIPS)) %>%
  select(id, zcta, tract_FIPS11)


##########
# Export #
##########
saveRDS(regards_geos, "data/regards_geos.rds")

