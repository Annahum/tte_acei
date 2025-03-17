
#-------------------------------------------------------------------------------

#ITT analysis

#------------------------------------------------------------------------------
#outcome
outcome_composite <- 1
  
#missing data
studypop_cat_missing <- 1

#causal estimand
itt <- 1

#confounders
confounder_full <- 1

#truncation of IPW
trunc <- 0

# time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

#run code

composite_itt <- func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main, n=500), 
                                  input_data =  cr_data_main,
                                  location_name = "outputs/composite/itt",
                                  txt_name_1 = "results_ipw_itt_rr_composite",
                                  txt_name_2 = "results_ipw_itt_hr_composite_5y",
                                  txt_name_3 = "Composite",
                                  txt_name_4 = "plot_ipw_itt_composite.pdf",
                                  txt_name_5 = "plot_ipw_itt_km_composite.pdf")

#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

#-------------------------------------------------------------------------------
#outcome
outcome_heartfailure <- 1

#missing data
studypop_cat_missing <- 1

#causal estimand
itt <- 1

#confounders
confounder_full <- 1

#truncation of IPW
trunc <- 0

# time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

hf_itt <- func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main, n=500), 
                                  input_data =  cr_data_main,
                                  location_name = "outputs/hf/itt",
                                  txt_name_1 = "results_ipw_itt_rr_hf",
                                  txt_name_2 = "results_ipw_itt_hr_hf_5y",
                                  txt_name_3 = "Heart Failure",
                                  txt_name_4 = "plot_ipw_itt_hf.pdf",
                                  txt_name_5 = "plot_ipw_itt_km_hf.pdf")

#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))
#-------------------------------------------------------------------------------
#outcome
outcome_death <- 1

#missing data
studypop_cat_missing <- 1

#causal estimand
itt <- 1

#confounders
confounder_full <- 1

#truncation of IPW
trunc <- 0

# time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

death_itt <- func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main, n=500), 
                                  input_data =  cr_data_main,
                                  location_name = "outputs/death/itt",
                                  txt_name_1 = "results_ipw_itt_rr_death",
                                  txt_name_2 = "results_ipw_itt_hr_death_5y",
                                  txt_name_3 = "Death",
                                  txt_name_4 = "plot_ipw_itt_death.pdf",
                                  txt_name_5 = "plot_ipw_itt_km_death.pdf")

#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))
#-------------------------------------------------------------------------------

#outcome
outcome_mi <- 1

#missing data
studypop_cat_missing <- 1

#causal estimand
itt <- 1

#confounders
confounder_full <- 1

#truncation of IPW
trunc <- 0

# time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

mi_itt <- func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main, n=500), 
                                  input_data =  cr_data_main,
                                  location_name = "outputs/mi/itt",
                                  txt_name_1 = "results_ipw_itt_rr_mi",
                                  txt_name_2 = "results_ipw_itt_hr_mi_5y",
                                  txt_name_3 = "MI",
                                  txt_name_4 = "plot_ipw_itt_mi.pdf",
                                  txt_name_5 = "plot_ipw_itt_km_mi.pdf")
#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))
#-------------------------------------------------------------------------------
# make overall image with four plots

combined_plot_itt <- (composite_itt | death_itt) / (hf_itt | mi_itt)
ggsave("itt_combined.pdf", path = "outputs") 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# time updated 

# for all analyses choose that the time updated IPW adherence weights are 
# calculated for both intervention arms together (botharms = F)
#-------------------------------------------------------------------------------

#outcome
outcome_composite <- 1

#missing data
studypop_cat_missing <- 1

#causal estimand
per_protocol <- 1
per_protocol_model1 <- 1

#confounders
confounder_full <- 1

#truncation of IPW
trunc <- 0

# time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

composite_pp <- func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main, n=500), 
                                  input_data =  cr_data_main,
                                  location_name = "outputs/composite/pp",
                                  txt_name_1 = "results_ipw_pp_rr_composite",
                                  txt_name_2 = "results_ipw_pp_hr_composite_5y",
                                  txt_name_3 = "Composite",
                                  txt_name_4 = "plot_ipw_pp_composite.pdf",
                                  txt_name_5 = "Not_required")

mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

#-------------------------------------------------------------------------------
#outcome
outcome_heartfailure <- 1

#missing data
studypop_cat_missing <- 1

#causal estimand
per_protocol <- 1
per_protocol_model1 <- 1

#confounders
confounder_full <- 1

#truncation of IPW
trunc <- 0

# time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

hf_pp <- func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main, n=500), 
                                  input_data =  cr_data_main,
                                  location_name = "outputs/hf/pp",
                                  txt_name_1 = "results_ipw_pp_rr_hf",
                                  txt_name_2 = "results_ipw_pp_hr_hf_5y",
                                  txt_name_3 = "Heart Failure",
                                  txt_name_4 = "plot_ipw_pp_hf.pdf",
                                  txt_name_5 = "Not_required")

mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

#-------------------------------------------------------------------------------
#outcome
outcome_death <- 1

#missing data
studypop_cat_missing <- 1

#causal estimand
per_protocol <- 1
per_protocol_model1 <- 1

#confounders
confounder_full <- 1

#truncation of IPW
trunc <- 0

# time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

death_pp <- func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main, n=500), 
                                  input_data =  cr_data_main,
                                  location_name = "outputs/death/pp",
                                  txt_name_1 = "results_ipw_pp_rr_death",
                                  txt_name_2 = "results_ipw_pp_hr_death_5y",
                                  txt_name_3 = "Death",
                                  txt_name_4 = "plot_ipw_pp_death.pdf",
                                  txt_name_5 = "Not_required")

mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

#-------------------------------------------------------------------------------
#outcome
outcome_mi <- 1

#missing data
studypop_cat_missing <- 1

#causal estimand
per_protocol <- 1
per_protocol_model1 <- 1

#confounders
confounder_full <- 1

#truncation of IPW
trunc <- 0

# time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

mi_pp <- func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main, n=500), 
                                  input_data =  cr_data_main,
                                  location_name = "outputs/mi/pp",
                                  txt_name_1 = "results_ipw_pp_rr_mi",
                                  txt_name_2 = "results_ipw_pp_hr_mi_5y",
                                  txt_name_3 = "MI",
                                  txt_name_4 = "plot_ipw_pp_mi.pdf",
                                  txt_name_5 = "Not_required")


mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

#------------------------------------------------------------------------------
# make overall image with four plots

combined_plot_pp <- (composite_pp | death_pp) / (hf_pp | mi_pp)
ggsave("pp_combined.pdf", path = "outputs") 
