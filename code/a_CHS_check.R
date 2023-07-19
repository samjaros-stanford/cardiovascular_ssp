##############
# Supplementary file to check for missingness across CHS, ZIP, Tract,
#   County, and ability to match to census variables
##############

library(haven)
library(stringr)
library(tidycensus)
library(tidyverse)

# Import data
outcomes = read.csv("raw_data/AbdelMagid_CHS_Data092822.csv") %>% mutate(idno = as.character(idno))
tract_geo = read_sas("raw_data/chs_census_baseline_2010.sas7bdat") %>% mutate(idno = as.character(idno))
zcta_geo = read_sas("raw_data/chs_zcta_baseline_2010.sas7bdat") %>% mutate(idno = as.character(idno))
tract_census = get_decennial(geography="tract", variables="H010001", state=unique(str_sub(tract_geo$t10_cen_uid_u_2010,1,2)), year=2010, show_call=T)
zcta_census = get_decennial(geography="zcta", variables="H010001", year=2010, show_call=T)

# Check matching between outcomes & geo files
out_geo_match = outcomes %>%
  select(idno) %>%
  mutate(inOutcomes = "inOutcomes") %>%
  full_join(tract_geo %>%
              select(idno) %>%
              mutate(inTracts = "inTracts"),
            by="idno") %>%
  full_join(zcta_geo %>%
              select(idno) %>%
              mutate(inZCTAs = "inZCTAs"),
            by="idno") %>%
  replace_na(list(inOutcomes="absentOutcome", inTracts="absentTracts", inZCTAs="absentZCTAs"))
table(out_geo_match$inOutcomes, out_geo_match$inTracts)
table(out_geo_match$inOutcomes, out_geo_match$inZCTAs)

# Check matching between geo & census files
tract_census_match = outcomes %>%
  select(idno) %>%
  mutate(inOutcomes = "inOutcomes") %>%
  full_join(tract_geo %>%
              select(idno, t10_cen_uid_u_2010) %>%
              mutate(inTracts = "inTracts"),
            by="idno") %>%
  left_join(tract_census %>%
              select(GEOID) %>%
              mutate(inCensus = "inCensus"),
            by=c("t10_cen_uid_u_2010"="GEOID")) %>%
  replace_na(list(inOutcomes="absentOutcome", inTracts="absentTracts", inCensus="absentCensus"))
table(tract_census_match$inTracts, tract_census_match$inCensus)
tract_census_match %>%
  group_by(inOutcomes, inTracts, inCensus) %>%
  summarize(count = n(),
            percent = n()/nrow(.))

zcta_census_match = outcomes %>%
  select(idno) %>%
  mutate(inOutcomes = "inOutcomes") %>%
  full_join(zcta_geo %>%
              select(idno, z10_cen_uid_u_2010) %>%
              mutate(inZCTAs = "inZCTAs"),
            by="idno") %>%
  left_join(zcta_census %>%
              select(GEOID) %>%
              mutate(inCensus = "inCensus"),
            by=c("z10_cen_uid_u_2010"="GEOID")) %>%
  replace_na(list(inOutcomes="absentOutcome", inZCTAs="absentZCTAs", inCensus="absentCensus"))
table(zcta_census_match$inZCTAs, zcta_census_match$inCensus)
zcta_census_match %>%
  group_by(inOutcomes, inZCTAs, inCensus) %>%
  summarize(count = n(),
            percent = n()/nrow(.))
