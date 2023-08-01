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
         gender = factor(gender, levels=c("Male", "Female")),
         race = factor(race, levels=c("White/Other", "Black")),
         educ = factor(educ, levels=c("College and above", "Some college", "High school", "Less than HS")),
         smoke = factor(smoke, levels=c("Never", "Former", "Current")),
         diabetes = factor(diabetes, levels=c("Normal", "Diabetes"))) %>%
  # Construct primary outcomes - Hypertension 1 & 2
  mutate(hbp1 = factor(if_else(sys_bp>=140 | dia_bp >=90, "HBP1", "Normal", missing=NA), levels=c("Normal","HBP1")),
         hbp2 = factor(if_else(sys_bp>=130 | dia_bp >=80, "HBP2", "Normal", missing=NA), levels=c("Normal","HBP2"))) %>%
  # Get ICE tertile & quantile values
  mutate(across(starts_with("ICE"), ~factor(ntile(.x, 3),levels=c(1:3)), .names="{.col}_tert")) %>%
  mutate(across(starts_with("ICE"), ~factor(ntile(.x, 5),levels=c(1:5)), .names="{.col}_quant"))

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
