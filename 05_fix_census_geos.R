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

library(stringr)
library(tidyverse)
# Set seed to make code reproducible
set.seed(1989)

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

# Cleaned chs data
chs_clean = readRDS("data/chs_cleaned.rds")

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
chs_analysis = chs_clean %>%
  rename(geo_tract11_yr2010 = geo_tract11) %>%
  rowwise() %>%
  mutate(geo_tract11 = transform_tract(geo_tract11_yr2010))
  
### Checks ###
# No one should change county
View(chs_analysis %>% filter(geo_county5!=str_sub(geo_tract11,1,5)))
# See changed tracts
View(chs_analysis %>% filter(geo_tract11_yr2010!=geo_tract11))

##########
# Export #
##########
saveRDS(chs_analysis %>% select(-ends_with("yr2010")),
        file="data/chs_analysis.rds")
