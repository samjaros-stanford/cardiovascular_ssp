
# Setup ########################################################################
library(here)
library(lme4)
library(lmerTest)
library(tidyverse)

# --- Cleaned & merged data import ---
# Scaling continuous variables due to large eigenvalues
full_analysis = as.data.frame(readRDS(here("data/full_analysis.rds")))
full_analysis$age_s = as.vector(scale(full_analysis$age))
full_analysis$bmi_s = as.vector(scale(full_analysis$bmi))
full_analysis$hdl_s = as.vector(scale(full_analysis$hdl))
full_analysis$ldl_s = as.vector(scale(full_analysis$ldl))
full_analysis$egfr_ckdepi_s = as.vector(scale(full_analysis$egfr_ckdepi))
full_analysis$crp_s = as.vector(scale(full_analysis$crp))

# --- Parameters for modeling ---
outcomes = c("hbp1", "sys_bp")
ICEs = c("ICEincome", "ICEraceeth", "ICEhome", "ICEincwnh", "ICEedu")
ICE_mods = c("", "_quint")
geo_levels = c("county", "zcta", "tract")
model_params = expand.grid("outcome"=outcomes, "ICE"=ICEs, "ICE_mod"=ICE_mods, "geo"=geo_levels,
                           stringsAsFactors=F)

# --- Parameters for saving ---
# Should all of the individual steps be saved to a csv?
# The final assembled dataset will always be saved
do_save_csv = FALSE
# 'type' will be replaced with the model being run
csv_file_name = "results/spatial_model_type_example.csv"

# --- Function to run model ---
# Accepts model specifications
# Returns data.frame with coefficients, CI, and p-val
do_mem = function(dataset, outcome, control_vars, ICE, ICE_mod, geo_level){
  # Create model formula
  # Ex: hbp1 ~ study + race + ICEedu_county_quint | (1|geo_county5)
  formula = paste0(outcome,
                   "~",control_vars,
                   ICE,"_",geo_level,ICE_mod,
                   "+(1|geo_",geo_level,ifelse(geo_level=="tract",11,5),")")
  
  # Run model based on outcome type
  # Numeric = use lmer, otherwise binomial)
  if(is.numeric(pull(full_analysis, all_of(outcome)))){
    mod = lmer(formula, data=dataset)
    ci = confint(mod, method="profile")
  } else {
    mod = glmer(formula, data=dataset, family=binomial, 
                # Allow for more iterations with the binomial regression due to
                #   convergence issues
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
    # Use Wald confidence intervals b/c confint would find lower deviances
    # Issues with Wald overcome by large sample size & p~=25%
    ci = confint(mod, method="Wald")
  }
  
  # Get needed parameters from model
  coef = summary(mod)$coefficients # All coefficients
  coef_names = row.names(coef)[startsWith(rownames(coef), "ICE")] # ICE coefficient names
  # Get ICE values from model
  ICEp = unname(coef[startsWith(rownames(coef), "ICE"), startsWith(colnames(coef), "Pr")])
  ICEest = unname(coef[startsWith(rownames(coef), "ICE"),"Estimate"])
  ICEci = matrix(ci[startsWith(row.names(ci), "ICE")], ncol=2, byrow=F)
  
  # Assemble ICE values and return data
  data.frame("geo"=geo_level, "ICE"=paste0(ICE,ICE_mod), 
             "outcome"=outcome, "coef_name"=coef_names, "est"=ICEest, 
             "l_ci"=ICEci[,1], "u_ci"=ICEci[,2], "p"=ICEp)
}

# Basic Model ##################################################################

# For basic model, just controlling for study
# NOTE, must end with +
basic_control_vars = "study+"

# Run models in parallel
basic_mods = data.frame()
for(i in 1:nrow(model_params)) {
  rbind(basic_mods,
        do_mem(data = full_analysis,
               outcome = model_params[i,1], 
               control_vars = basic_control_vars, 
               ICE = model_params[i,2],
               ICE_mod = model_params[i,3],
               geo_level = model_params[i,4]))
}

# Post-processing to join parameters with results
basic_mods_wMissing = model_params %>%
  mutate(ICE = paste0(ICE, ICE_mod)) %>%
  select(-ICE_mod) %>%
  full_join(basic_mods, by=c("outcome", "ICE", "geo")) %>%
  mutate(model="basic")

# Save file separately
if(do_save_csv){
  write.csv(basic_mods_wMissing, 
            file=here("data", str_replace(csv_file_name, "type", "basic")), 
            row.names=F)
}

# Demographics Model ###########################################################

# For demographics model, control for study & demographics
# NOTE, must end with +
demog_control_vars = "study+age_s+gender+race+educ+"

# Run models in parallel
demo_mods = data.frame()
for(i in 1:nrow(model_params)) {
  rbind(demo_mods,
        do_mem(data = full_analysis,
               outcome = model_params[i,1], 
               control_vars = demog_control_vars, 
               ICE = model_params[i,2],
               ICE_mod = model_params[i,3],
               geo_level = model_params[i,4]))
}

# Post-processing to join parameters with results
demo_mods_wMissing = model_params %>%
  mutate(ICE = paste0(ICE, ICE_mod)) %>%
  select(-ICE_mod) %>%
  full_join(demo_mods, by=c("outcome", "ICE", "geo")) %>%
  mutate(model="demog")

# Save file separately
if(do_save_csv){
  write.csv(demo_mods_wMissing, 
            file=here("data", str_replace(csv_file_name, "type", "demo")), 
            row.names=F)
}

# Full model ###################################################################

# For full model, control for study & demographics
# NOTE, must end with +
full_control_vars = "study+age_s+gender+race+educ+diabetes+smoke+mi+bmi_s+hdl_s+ldl_s+egfr_ckdepi_s+crp_s+"

# Run models in parallel
full_mods = data.frame()
for(i in 1:nrow(model_params)) {
  rbind(full_mods,
        do_mem(data = full_analysis,
               outcome = model_params[i,1], 
               control_vars = full_control_vars, 
               ICE = model_params[i,2],
               ICE_mod = model_params[i,3],
               geo_level = model_params[i,4]))
}

# Post-processing to join parameters with results
full_mods_wMissing = model_params %>%
  mutate(ICE = paste0(ICE, ICE_mod)) %>%
  select(-ICE_mod) %>%
  full_join(full_mods, by=c("outcome", "ICE", "geo")) %>%
  mutate(model="full")

# Save file separately
if(do_save_csv){
  write.csv(full_mods_wMissing, 
            file=here("data", str_replace(csv_file_name, "model", "full")), 
            row.names=F)
}

# Combine Results ##############################################################

rbind(basic_mods_wMissing, demo_mods_wMissing, full_mods_wMissing) %>%
  mutate(across(c("est","l_ci","u_ci"), 
                ~round(if_else(grepl("hbp", outcome), exp(.x) , .x), 4)),
         p = round(p, 4)) %>%
  write.csv(file=here("data", str_replace(csv_file_name, "type", "full")), 
            row.names=F)
