library(tidyverse)

##############
# Processing #
##############
# Assemble CHS & REGARDS into final analysis dataset
full_analysis = 
  # Stack harmonized CHS & REGARDS
  bind_rows(readRDS("data/chs_cleaned.rds") %>%
              select(-starts_with("geo_")) %>%
              left_join(readRDS("data/chs_imputed_geos.rds")),
            readRDS("data/regards_cleaned.rds") %>%
              select(-starts_with("geo_")) %>%
              left_join(readRDS("data/regards_imputed_geos.rds"))) %>%
  ungroup() %>%
  # Merge in 2000 census
  left_join(readRDS("data/ICE/ice_county_dec.rds"), by=c("geo_county5"="GEOID")) %>%
  left_join(readRDS("data/ICE/ice_zcta_dec.rds"), by=c("geo_zcta5"="GEOID")) %>%
  left_join(readRDS("data/ICE/ice_tract_dec.rds"), by=c("geo_tract11"="GEOID")) %>%
  # Construct primary outcomes - Hypertension 1 & 2
  mutate(hbp1 = sys_bp>=140 | dia_bp >=90,
         hbp2 = sys_bp>=130 | dia_bp >=80) %>%
  # Get ICE tertile & quantile values
  mutate(across(starts_with("ICE"), 
                .fns=~factor(ntile(.x, 5),levels=c(5:1)),
                .names="{.col}_quint")) %>%
  rename_with(~str_replace_all(.x, "_raw", ""), starts_with("ICE"))
  
# Factor variables as needed
full_analysis$id = factor(full_analysis$id)
full_analysis$study = factor(full_analysis$study, levels=c("CHS", "REGARDS"))
full_analysis$gender = factor(full_analysis$gender, levels=c("Male", "Female"))
full_analysis$race = factor(full_analysis$race, levels=c("White/Other", "Black"))
full_analysis$educ = factor(full_analysis$educ, levels=c("College and above", "Some college", "High school", "Less than HS"))
full_analysis$diabetes = factor(full_analysis$diabetes, levels=c("Normal", "Diabetes"))
full_analysis$stroke = factor(full_analysis$stroke, levels=c("No Stroke", "Stroke"))
full_analysis$mi = factor(full_analysis$mi, levels=c("No MI", "MI"))
full_analysis$htn_med = factor(full_analysis$htn_med, levels=c("No Meds", "Meds"))
full_analysis$smoke = factor(full_analysis$smoke, levels=c("Never", "Former", "Current"))

# Get missingness
print(full_analysis %>%
        #filter(study=="CHS") %>%
        ungroup() %>%
        summarize(across(everything(), ~mean(is.na(.x))*100)) %>%
        pivot_longer(cols = everything(),
                     names_to = "col",
                     values_to = "pct_missing"),
      n=ncol(full_analysis))

##########
# Export #
##########
saveRDS(full_analysis, "data/full_analysis.rds")
