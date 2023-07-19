library(stringr)
library(tidycensus)
library(tidyverse)

# Get function from calculate_ice project
source("C:/Users/samja/OneDrive - Stanford/calculate_ice/R/calculate_ice.R")

chs_geos = readRDS("data/chs_geos.rds")
regards_geos = readRDS("data/regards_geos.rds")

needed_zcta = unique(c(chs_geos$zcta, regards_geos$zcta))
zcta_ICE = create_ice(needed_zcta, "zcta", api_key=paste(readLines("secret.txt")), year=2010, keep_acs=F, cache_acs=T)

needed_tracts = unique(c(chs_geos$tract_FIPS11, regards_geos$tract_FIPS11))

a=get_decennial(geography="zcta", variables="PCT012H100", year=2010, show_call=T)
b=chs_geos %>%
  left_join(a, by=c("zcta"="GEOID"))
sum(!is.na(b$value))/5888
c=regards_geos %>%
  left_join(a, by=c("zcta"="GEOID"))
sum(!is.na(c$value))/30183

d=get_decennial(geography="tract", variables="PCT012H100", state=unique(str_sub(needed_tracts,1,2)), year=2010, show_call=T)
e=chs_geos %>%
  left_join(d, by=c("tract_FIPS11"="GEOID"))
sum(!is.na(e$value))/5888
f=regards_geos %>%
  left_join(d, by=c("tract_FIPS11"="GEOID"))
sum(!is.na(f$value))/30183
