library(tidyverse)

##############
# Processing #
##############
# Assemble CHS & REGARDS into final analysis dataset
full_analysis = rbind(
  # Stack harmonized CHS & REGARDS
  # Merge in proper census (2000 decennial for CHS & combined 2010-12 ACS for REGARDS)
  readRDS("data/chs_analysis.rds") %>% 
    left_join(readRDS("data/ICE/ice_county_acs.rds"), by=c("geo_county5"="GEOID")) %>%
    left_join(readRDS("data/ICE/ice_zcta_acs.rds"), by=c("geo_zcta5"="GEOID")) %>%
    left_join(readRDS("data/ICE/ice_tract_acs.rds"), by=c("geo_tract11"="GEOID")), 
  readRDS("data/regards_analysis.rds") %>%
    left_join(readRDS("data/ICE/ice_county_dec.rds"), by=c("geo_county5"="GEOID")) %>%
    left_join(readRDS("data/ICE/ice_zcta_dec.rds"), by=c("geo_zcta5"="GEOID")) %>%
    left_join(readRDS("data/ICE/ice_tract_dec.rds"), by=c("geo_tract11"="GEOID"))) %>%
  # Factor variables as needed
  mutate(id = factor(id),
         study = factor(study, levels=c("CHS", "REGARDS")),
         ### Demographics
         gender = factor(gender, levels=c("Male", "Female")),
         race = factor(race, levels=c("White/Other", "Black")),
         educ = factor(educ, levels=c("College and above", "Some college", "High school", "Less than HS")),
         ### Medical history
         diabetes = factor(diabetes, levels=c("Normal", "Diabetes")),
         stroke = factor(stroke, levels=c("No Stroke", "Stroke")),
         mi = factor(mi, levels=c("No MI", "MI")),
         ### Comorbidities
         smoke = factor(smoke, levels=c("Never", "Former", "Current"))) %>%
  # Construct primary outcomes - Hypertension 1 & 2
  mutate(hbp1 = sys_bp>=140 | dia_bp >=90,
         hbp2 = sys_bp>=130 | dia_bp >=80) %>%
  # Get ICE tertile & quantile values
  mutate(across(starts_with("ICE"), 
                .fns=list(tert=~factor(ntile(.x, 3),levels=c(3:1)), quant=~factor(ntile(.x, 5),levels=c(5:1))),
                .names="{.col}_{.fn}"))

# Get missingness
print(full_analysis %>%
        #filter(study=="CHS") %>%
        summarize(across(everything(), ~mean(is.na(.x))*100)) %>%
        pivot_longer(cols = everything(),
                     names_to = "col",
                     values_to = "pct_missing"),
      n=ncol(full_analysis))

##########
# Export #
##########
saveRDS(full_analysis, "data/full_analysis.rds")
