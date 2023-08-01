library(haven)
library(tidyverse)

##########
# Import #
##########
# Baseline data from Hoda
#   1 patient/line
#   ID = idno
raw_chs = read.csv("raw_data/AbdelMagid_CHS_Data092822.csv")
# Tract data from Hoda
#   1 patient/line
#   ID = idno
raw_chs_tract = read_sas("raw_data/chs_census_baseline_2010.sas7bdat")
# ZCTA data from Hoda
#   1 patient/line
#   ID = idno
raw_chs_zcta = read_sas("raw_data/chs_zcta_baseline_2010.sas7bdat")

##############
# Processing #
##############
# Retrieve CHS data needed for analysis
# This section only harmonizes the data in preparation for merging with
#   geography, ICEs, and REGARDS
chs_analysis = raw_chs %>%
  # Merge in geographic data
  left_join(raw_chs_tract %>%
              select(idno, t10_cen_uid_u_2010) %>%
              rename(geo_tract11=t10_cen_uid_u_2010),
            by="idno") %>%
  mutate(geo_county5=str_sub(geo_tract11, 1, 5)) %>%
  left_join(raw_chs_zcta %>%
              select(idno, z10_cen_uid_u_2010) %>%
              rename(geo_zcta5=z10_cen_uid_u_2010),
            by="idno") %>%
  # Harmonize variables needed for analysis
  #   Add a C to the patient ID for CHS and add a study identifier column
  #   CHS was recruited in 2 waves - The baseline for wave 1 is year 2, wave 2 is year 5
  mutate(id = paste0("C", idno),
         study = "CHS",
         age = agebl,
         # Harmonize demographics to match REGARDS
         #   Gender = Male/Female
         gend01 = str_trim(gend01),
         gender = case_when(
           gend01=="male"   ~ "Male",
           gend01=="female" ~ "Female",
           T                ~ NA_character_),
         #   Education = Less than HS, High School, Some College, College and above
         grade01 = str_trim(grade01),
         educ = case_when(
           grade01 == "no schooling"             ~ "Less than HS",
           grade01 == "grade 12"                 ~ "High school",
           grade01 == "ged"                      ~ "High school",
           grade01 == "4 years college"          ~ "College and above",
           grade01 == "graduate or professional" ~ "College and above",
           grepl("grade", grade01)               ~ "Less than HS",
           grepl("college", grade01)             ~ "Some college",
           grepl("vocational", grade01)          ~ "High school",
           T                                     ~ NA_character_),
         #   Race = Odden collapses to Black vs White/Other due to most patients being White or Black
         race = case_when(
           race01 == "black"                     ~ "Black",
           grepl("^(?!\\s*$).+", race01, perl=T) ~ "White/Other",
           T                                     ~ NA_character_),
         #   Smoke = 1-Never, 2-Former, 3-Current
         smoke = if_else(perstat=="old cohort", smoke2, smoke5),
         smoke = case_when(
           smoke == 1 ~ "Never",
           smoke == 2 ~ "Former",
           smoke == 3 ~ "Current",
           T          ~ NA_character_),
         #   Diabetes = Yes - 1/No - 0
         diabetes = case_when(
           diabet==1 ~ "Diabetes",
           diabet==0 ~ "Normal",
           T         ~ NA_character_),
         # Select proper timepoint based on wave
         sys_bp   =if_else(perstat=="old cohort",avesys2,  avesys5), 
         dia_bp   =if_else(perstat=="old cohort",avedia2,  avedia5), 
         bmi      =if_else(perstat=="old cohort",bmi2,     bmi5),
         hdl      =if_else(perstat=="old cohort",hdl2,     hdl5),
         ldl      =if_else(perstat=="old cohort",ldladj2,  ldladj5),
         crp      =if_else(perstat=="old cohort",crp2,     crp5)) %>%
  # Calculate CKD-EPI as described by Levey et al. 2009
  mutate(scr   = if_else(perstat=="old cohort", cre2clb, cre5clb),
         kappa = if_else(gender=="female",      0.7,     0.9),
         alpha = if_else(gender=="female",      -0.329,  -0.411),
         final = if_else(gender=="female",      1.018,   1)*
                 if_else(race=="black",         1.159,   1)) %>%
  rowwise() %>%
  mutate(egfr_ckdepi = 141*min(scr/kappa,1)^alpha*max(scr/kappa,1)^-1.209*0.993^age*final) %>%
  ungroup() %>%
  # Keep only variables of interest
  select(id, study, sys_bp, dia_bp, age, gender, race, educ, smoke, bmi, 
         hdl, ldl, diabetes, egfr_ckdepi, crp, starts_with("geo_"))

# Get missingness
chs_analysis %>%
  summarize(across(everything(), ~sum(is.na(.x))/nrow(chs_analysis)*100)) %>%
  pivot_longer(cols = everything(),
               names_to = "col",
               values_to = "pct_missing")

##########
# Export #
##########
saveRDS(chs_analysis, "data/chs_analysis.rds")

