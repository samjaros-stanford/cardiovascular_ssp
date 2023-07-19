##############
# Supplementary file to check for missingness across REGARDS, ZIP, Tract,
#   County, and ability to match to census variables
##############
library(tidycensus)
library(tidyverse)

# Import data
outcomes = read.csv("raw_data/REGARDS_BASELINE_DATASET.csv") %>% mutate(id = as.character(id))
tract_census00 = get_decennial(geography="tract", variables="H010001", state=unique(outcomes$GEO_StateFIPS), year=2000, show_call=T)
zcta_census00 = get_decennial(geography="zcta", variables="H010001", year=2000, show_call=T)
tract_census10 = get_decennial(geography="tract", variables="H010001", state=unique(outcomes$GEO_StateFIPS), year=2010, show_call=T)
zcta_census10 = get_decennial(geography="zcta", variables="H010001", year=2010, show_call=T)

# Check missingness among outcomes & census files
out_geo_missingness = outcomes %>%
  select(id, GEO_Zip, GEO_StateFIPS, GEO_CountyFIPS, GEO_Tract) %>%
  mutate(inOutcomes = "yesOutcomes",
         inZCTAs = case_when(
           grepl("[0-9]{5}", GEO_Zip, perl=T)       ~ "valid5",
           grepl("[0-9]{2,3}x{2}", GEO_Zip, perl=T) ~ "valid3",
           T                                        ~ "noZCTA"),
         inStates = case_when(
           grepl("[0-9]{1,2}", GEO_StateFIPS, perl=T) ~ "validState",
           T                                          ~ "noState"),
         # Some of the counties seem to be place (city) codes, numbers >500
         inCounties = case_when(
           grepl("[0-9]{1,3}", GEO_CountyFIPS, perl=T) ~ "validCounty",
           T                                               ~ "noCounty"),
         inTracts = case_when(
           grepl("[0-9]{1,6}", GEO_Tract, perl=T) ~ "validTract",
           T                                      ~ "noTract")) %>%
  mutate(c_ZCTA = str_pad(GEO_Zip,5,"left","0"),
         c_FIPS11 = str_c(str_pad(GEO_StateFIPS,2,"left","0"),
                        str_pad(GEO_CountyFIPS,3,"left","0"),
                        str_pad(GEO_Tract,6,"left","0")))
table(out_geo_missingness$inOutcomes, out_geo_missingness$inZCTAs)
table(out_geo_missingness$inOutcomes, out_geo_missingness$inTracts)

# Tract missingness table
out_geo_missingness %>%
  left_join(tract_census10 %>%
              select(GEOID) %>%
              mutate(inCensus = "inCensus"),
            by=c("c_FIPS11"="GEOID")) %>%
  replace_na(list(inOutcomes="absentOutcome", inTracts="absentTracts", inCensus="absentCensus")) %>%
  #filter(inTracts=="validTract" & inCensus=="absentCensus")
  group_by(inOutcomes, inTracts, inCensus) %>%
  summarize(count = n(),
            percent = n()/nrow(.))

# ZCTA missingness table
out_geo_missingness %>%
  left_join(zcta_census10 %>%
              select(GEOID) %>%
              mutate(inCensus = "inCensus"),
            by=c("c_ZCTA"="GEOID")) %>%
  replace_na(list(inOutcomes="absentOutcome", inZCTAs="noZCTA", inCensus="absentCensus")) %>%
  #filter(inZCTAs!="noZCTA" & inCensus=="absentCensus")
  group_by(inOutcomes, inZCTAs, inCensus) %>%
  summarize(count = n(),
            percent = n()/nrow(.))
