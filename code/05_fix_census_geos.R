# CHS uses 2010 census tract definitions
# REGARDS uses 2000 census tract definitions
# These definitions are technically not the same, so we need to change one of
#   them before combining the data sets.

# We will change the CHS to 2000 census tract definitions because there are many
#   more REGARDS participants than CHS participants. Also, 2000 is more relevant
#   to both cohorts as it is in between the collection time frames of both.

# To merge, we will use the census 2000 to 2010 crosswalk file and assign the
#   CHS participants new census tracts based on the new census tract if it is a
#   1-to-1 conversion or census tract split; or we will use a 
#   population-weighted dice roll for census tracts that were combined. The
#   crosswalk file contains the field POPPCT10 which describes the percent of
#   the 2010 population that lives in that intersection of 2000 and 2010 tracts.
#   This percentage is used in the weighted coin flip to determine which of the
#   adjacent tracts that patient is assigned to.

# While this imputation method will introduce some random error, this
#   method only determines which of two neighboring census tracts the patient
#   will be placed in. This method can be repeated to simulate different
#   assignment possibilities in a sensitivity analysis.

# This method is repeated for assigning ZCTAs based on the 2010 HUD crosswalk
#   file.

library(tidyverse)
# Set seed to make code reproducible
set.seed(1989)

## --- Correct tracts in CHS ---------------------------------------------------

##########
# Import #
##########

## 2000-2010 CROSSWALK ###
# Files to transform 2010 geographies to 2000 geographies so that CHS and REGARDS are comparable
# 2000<->2010 Tracts, note column names are stored separately
# Code to generate crosswalk file is commented out to save on API hits

# crosswalk_tract = read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/trf_txt/us2010trf.txt", col_names=F)
# colnames(crosswalk_tract) = names(read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/trfheader.txt", col_names=T))
# saveRDS(crosswalk_tract, file="data/tract_crosswalk.rds")
crosswalk_tract = readRDS("data/tract_crosswalk.rds")

# Cleaned CHS data from 01_clean_CHS.R
chs_geos = readRDS("data/chs_cleaned.rds") %>%
  select(id, starts_with("geo_"))

##############
# Processing #
##############

# Get list of 2010 tracts and their associated 2000 tracts and probabilities
crosswalk_tract_specs = crosswalk_tract %>%
  # Remove tracts that have none of the 2010 population
  filter(POPPCT10>0) %>%
  group_by(GEOID10) %>%
  summarize(tracts=list(GEOID00),
            probs=list(POPPCT10/100)) %>%
  # For speed, remove rows that have a probably of 1 and where the new and old tract are the same
  rowwise() %>%
  filter(length(probs)>1 || GEOID10!=tracts[1]) %>%
  # Combine tracts and probs into a list for each 2010 census tract
  mutate(specs00 = list(list("tracts"=tracts, "probs"=probs)))
# Extract list and name elements
crosswalk_tract_list = setNames(crosswalk_tract_specs$specs00, crosswalk_tract_specs$GEOID10)

# Function taking a 2010 tract and returning a 2000 tract
#   If the tract is in the crosswalk file, roll a weighted dice based on the
#     population percentages
#   If the tract is not in the crosswalk file, it stays the same
transform_tract = function(tract10){
  if(exists(tract10, where=crosswalk_tract_list)){
    this_tract = crosswalk_tract_list[[tract10]]
    if(this_tract$probs[1]==1){
      return(this_tract$tracts[1])
    }
    return(sample(this_tract$tracts,size=1,prob=this_tract$probs))
  }
  return(tract10)
}

# Use function and translator list to pick a new tract for each CHS participant
chs_new_tract = chs_geos %>%
  rename(geo_tract11_yr2010 = geo_tract11) %>%
  rowwise() %>%
  mutate(geo_tract11 = transform_tract(geo_tract11_yr2010))
  
### Checks ###
# No one should change county
nrow(filter(chs_new_tract, geo_county5!=str_sub(geo_tract11,1,5)))
# Percent changed tracts
nrow(filter(chs_new_tract, geo_tract11_yr2010!=geo_tract11)) / 
  nrow(chs_new_tract)

## --- Correct ZIPs in both using HUD crosswalk -------------------------------

# Remove all crosswalk variables from memory
rm(list=ls()[startsWith(ls(),"crosswalk")])

##########
# Import #
##########

# HUD crosswalk file describing the percent of each tract in each ZIP
#   Source: https://www.huduser.gov/portal/datasets/usps_crosswalk.html
crosswalk_tract_to_zip = read_csv("raw_data/TRACT_ZIP_032010.csv")

# REGARDS cleaned data from 02_clean_REGARDS.R
regards_geos = readRDS("data/regards_cleaned.rds") %>%
  select(id, starts_with("geo_"))

##############
# Processing #
##############

# Get list of 2000 tracts and their associated 2010 ZIPs and probabilities
crosswalk_tract_to_zip_specs = crosswalk_tract_to_zip %>%
  # For speed and to save memory, remove all ZIP rows that have no chance of
  #   being assigned
  filter(RES_RATIO>0) %>%
  group_by(TRACT) %>%
  summarize(zips=list(ZIP),
            probs=list(RES_RATIO)) 
# Create list of these values for easy lookup
crosswalk_tract_to_zip_list = setNames(
  mapply(list, "zips"=crosswalk_tract_to_zip_specs$zips, 
         "probs"=crosswalk_tract_to_zip_specs$probs, SIMPLIFY=F), 
  crosswalk_tract_to_zip_specs$TRACT)

# Function taking a 2000 tract and returning a 2010 ZIP
#   If the tract is in the crosswalk file, roll a weighted dice based on the
#     population percentages
transform_zip = function(tract00){
  if(exists(tract00, where=crosswalk_tract_to_zip_list)){
    this_tract = crosswalk_tract_to_zip_list[[tract00]]
    if(length(this_tract$probs)==1){
      return(this_tract$zips[1])
    }
    return(sample(this_tract$zips,size=1,prob=this_tract$probs))
  }
  return(NA_character_)
}

# Use function and translator list to pick a new ZIP for each participant
chs_new_geos = chs_new_tract %>%
  rename(geo_zcta5_old = geo_zcta5) %>%
  rowwise() %>%
  mutate(geo_zcta5 = transform_zip(geo_tract11)) %>%
  # If the new ZCTA is missing but the old isn't, impute new with old
  ungroup() %>%
  mutate(geo_zcta5 = if_else(is.na(geo_zcta5),geo_zcta5_old,geo_zcta5))

regards_new_geos = regards_geos %>%
  rename(geo_zcta5_old = geo_zcta5) %>%
  rowwise() %>%
  mutate(geo_zcta5 = transform_zip(geo_tract11)) %>%
  # If the new ZCTA is missing but the old isn't, impute new with old
  ungroup() %>%
  mutate(geo_zcta5 = if_else(is.na(geo_zcta5),geo_zcta5_old,geo_zcta5))

### Checks ###
# Percent CHS ZCTAs changed
nrow(filter(chs_new_geos, geo_zcta5_old!=geo_zcta5)) / nrow(chs_new_geos)
# Percent REGARDS ZCTAs changed
nrow(filter(regards_new_geos, geo_zcta5_old!=geo_zcta5)) / nrow(regards_new_geos)

##########
# Export #
##########
saveRDS(chs_new_geos %>% select(-ends_with(c("old", "yr2010"))),
        file="data/chs_imputed_geos.rds")
saveRDS(regards_new_geos %>% select(-ends_with("old")),
        file="data/regards_imputed_geos.rds")


