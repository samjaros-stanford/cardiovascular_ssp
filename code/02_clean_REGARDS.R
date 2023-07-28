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
         geo_tract11 = str_c(geo_county5, str_pad(GEO_Tract,6,"left","0"))) %>%
  # Rename & select variables needed for analysis
  mutate(id=paste0("R", id),
         study="REGARDS",
         # Harmonize demographics to match CHS
         #   Gender = Male/Female
         gender = case_when(
           Gender=="F" ~ "Female",
           Gender=="M" ~ "Male",
           T           ~ NA_character_),
         #   Education = Less than HS, High School, Some College, College and above
         educ = case_when(
           ED_Cat == "Less than high school"      ~ "Less than HS",
           ED_Cat == "High school graduate"       ~ "High school",
           ED_Cat == "Some college"               ~ "Some college",
           ED_Cat == "College graduate and above" ~ "College and above",
           T                                      ~ NA_character_),
         #   Race = Black/White
         race = case_when(
           Race == "B" ~ "Black",
           Race == "W" ~ "White",
           T           ~ NA_character_),
         #   Smoker = Never/Former/Current
         smoke = case_when(
           Smoke == "Never"   ~ "Never",
           Smoke == "Past"    ~ "Former",
           Smoke == "Current" ~ "Current",
           T                  ~ NA_character_),
         #   Diabetes = Diabetes/Normal
         diabetes = case_when(
           Diabetes_SR == "Y" ~ "Diabetes",
           Diabetes_SR == "N" ~ "Normal",
           T                  ~ NA_character_)) %>%
  rename(sys_bp=SBP, 
         dia_bp=DBP, 
         age=Age, 
         alcohol=Alc_Drinks_Wk, 
         bmi=BMI, 
         hdl=Hdl, 
         ldl=Ldl, 
         egfr_ckdepi=EGFR_CKDEPI,
         crp=Crp) %>%
  select(id, study, sys_bp, dia_bp, age, gender, race, educ, alcohol, smoke, bmi, 
         hdl, ldl, diabetes, egfr_ckdepi, crp, starts_with("geo_", ignore.case=F))

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

