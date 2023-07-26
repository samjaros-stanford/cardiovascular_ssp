library(tidyverse)

##########
# Import #
##########
# Baseline data from Hoda
#   Already 1 person/line
#   ID = id
raw_regards = read.csv("raw_data/REGARDS_BASELINE_DATASET.csv")

##############
# Processing #
##############
# Retrieve REGARDS data needed for analysis
# This section only harmonizes data in preparation for merging with ICE and CHS
#   data.
regards_analysis = raw_regards %>%
  # Format geography data
  mutate(geo_zcta5 = if_else(grepl("[0-9]{5}", GEO_Zip, perl=T), GEO_Zip, NA_character_),
         geo_county5 = str_c(str_pad(GEO_StateFIPS,2,"left","0"), str_pad(GEO_CountyFIPS,3,"left","0")), 
         tract_FIPS11 = str_c(geo_county5, str_pad(GEO_Tract,6,"left","0"))) %>%
  # Rename & select variables needed for analysis
  # No Cystatain C in REGARDS right now
  mutate(id=paste0("R", id),
         study="REGARDS",
         cyst_c=NA) %>%
  rename(sys_bp=SBP, 
         dia_bp=DBP, 
         age=Age, 
         gender=Gender, 
         race=Race, 
         educ=ED_Cat, 
         alcohol=Alc_Drinks_Wk, 
         smoke=Smoke, 
         bmi=BMI, 
         hdl=Hdl, 
         ldl=Ldl, 
         diabetes=Diabetes_SR, 
         #cyst_c=cysc, 
         crp=Crp) %>%
  select(id, study, sys_bp, dia_bp, age, gender, race, educ, alcohol, smoke, bmi, 
         hdl, ldl, diabetes, cyst_c, crp, starts_with("geo_"))

# Get missingness
print(regards_analysis %>%
        summarize(across(everything(), ~sum(is.na(.x))/nrow(regards_analysis)*100)) %>%
        pivot_longer(cols = everything(),
                     names_to = "col",
                     values_to = "pct_missing"),
      n=ncol(regards_analysis))

##########
# Export #
##########
saveRDS(regards_analysis, "data/regards_analysis.rds")

