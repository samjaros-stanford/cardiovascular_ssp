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
# Get tracts & ZCTAs needed for ICE calcs
chs_analysis = raw_chs %>%
  # Rename & select variables needed for analysis
  # No education variable in our CHS dataset right now
  mutate(education = NA) %>%
  rename(sys_bp=avesys2, 
         dia_bp=avedia2, 
         age=agebl, 
         gender=gend01, 
         race=race01, 
         educ=education, 
         alcohol=alcoh2, 
         smoke=smoke2, 
         bmi=bmi2, 
         hdl=hdl2, 
         ldl=ldladj2, 
         diabetes=diabada2, 
         cyst_c=cystatc2, 
         crp=crp2) %>%
  select(idno, sys_bp, dia_bp, age, gender, race, educ, alcohol, smoke, bmi, 
         hdl, ldl, diabetes, cyst_c, crp) %>%
  # Merge in geographic data
  left_join(raw_chs_tract %>%
              select(idno, t10_cen_uid_u_2010) %>%
              rename(geo_tract11=t10_cen_uid_u_2010),
            by="idno") %>%
  mutate(geo_county5=str_sub(geo_tract11, 1, 5)) %>%
  left_join(raw_chs_zcta %>%
              select(idno, z10_cen_uid_u_2010) %>%
              rename(geo_zcta5=z10_cen_uid_u_2010),
            by="idno")

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
