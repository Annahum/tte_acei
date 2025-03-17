#-------------------------------------------------------------------------------
# sensitivity analyses for composite outcome

mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

#-------------------------------------------------------------------------------
# complete case analysis for ITT effect (IPW)

#outcome
outcome_composite <- 1

#missing data
studypop_completecase <- 1

#causal estimand
itt <- 1

#confounders
confounder_full <- 1

#truncation of IPW
trunc <- 0

#time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main, n=200), 
                                  input_data =  cr_data_main,
                                  location_name = "outputs/composite/sensitivity/complete_case",
                                  txt_name_1 = "results_ipw_itt_rr_composite_cc",
                                  txt_name_2 = "results_ipw_itt_hr_composite_5y_cc",
                                  txt_name_3 = "ITT composite_cc",
                                  txt_name_4 = "plot_ipw_itt_composite_cc.pdf",
                                  txt_name_5 = "plot_ipw_itt_km_composite_cc.pdf")
#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

#-------------------------------------------------------------------------------
# impute median for missing data ITT effect

#outcome
outcome_composite <- 1

#missing data
studypop_median <- 1

#causal estimand
itt <- 1

#confounders
confounder_full <- 1

#truncation of IPW
trunc <- 0

#time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main, n=200), 
                                  input_data =  cr_data_main,
                                  location_name = "outputs/composite/sensitivity/median",
                                  txt_name_1 = "results_ipw_itt_rr_composite_median",
                                  txt_name_2 = "results_ipw_itt_hr_composite_5y_median",
                                  txt_name_3 = "ITT composite_median",
                                  txt_name_4 = "plot_ipw_itt_composite_median.pdf",
                                  txt_name_5 = "plot_ipw_itt_km_composite_median.pdf")
#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

#-------------------------------------------------------------------------------
# truncation of IPW (99%) ITT effect

#outcome
outcome_composite <- 1

#missing data
studypop_cat_missing <- 1

#causal estimand
itt <- 1

#confounders
confounder_full <- 1

#truncation of IPW
trunc <- 1

#time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main), 
                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main, n=200), 
                  input_data =  cr_data_main,
                  location_name = "outputs/composite/sensitivity/truncation",
                  txt_name_1 = "results_ipw_itt_rr_composite_truncation",
                  txt_name_2 = "results_ipw_itt_hr_composite_5y_truncation",
                  txt_name_3 = "ITT composite_truncation",
                  txt_name_4 = "plot_ipw_itt_composite_truncation.pdf",
                  txt_name_5 = "plot_ipw_itt_km_composite_truncation.pdf")
#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

#------------------------------------------------------------------------------
# coronary artery stenosis restriction
load("W:/C6_Berglund/data/procdata/angiotensin/new_swedeheart/cr_data_main_fynd.Rda") #load dataset

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

#time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main_fynd), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main_fynd, n=200), 
                                  input_data =  cr_data_main_fynd,
                                  location_name = "outputs/composite/sensitivity/fynd",
                                  txt_name_1 = "results_ipw_itt_rr_composite_fynd",
                                  txt_name_2 = "results_ipw_itt_hr_composite_5y_fynd",
                                  txt_name_3 = "ITT composite_fynd",
                                  txt_name_4 = "plot_ipw_itt_composite_fynd.pdf",
                                  txt_name_5 = "plot_ipw_itt_km_composite_fynd.pdf")
#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))
rm(cr_data_main_fynd)
#-----------------------------------------------------------------------------
# restriction to the years prior to covid
load("W:/C6_Berglund/data/procdata/angiotensin/new_swedeheart/cr_data_main_covid.Rda") #load data

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

#time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main_covid), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main_covid, n=200), 
                                  input_data =  cr_data_main_covid,
                                  location_name = "outputs/composite/sensitivity/covid",
                                  txt_name_1 = "results_ipw_itt_rr_composite_covid",
                                  txt_name_2 = "results_ipw_itt_hr_composite_5y_covid",
                                  txt_name_3 = "ITT composite_covid",
                                  txt_name_4 = "plot_ipw_itt_composite_covid.pdf",
                                  txt_name_5 = "plot_ipw_itt_km_composite_covid.pdf")
#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))
rm(cr_data_main_covid)
#-----------------------------------------------------------------------------
# restriction with few antithrombotics
load("W:/C6_Berglund/data/procdata/angiotensin/new_swedeheart/cr_data_main_antit_restriction.Rda") #load data

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

#time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main_antit_restriction), 
                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main_antit_restriction, n=200), 
                  input_data =  cr_data_main_antit_restriction,
                  location_name = "outputs/composite/sensitivity/antit_restriction",
                  txt_name_1 = "results_ipw_itt_rr_composite_antit_restriction",
                  txt_name_2 = "results_ipw_itt_hr_composite_5y_antit_restriction",
                  txt_name_3 = "ITT composite_antit_restriction",
                  txt_name_4 = "plot_ipw_itt_composite_antit_restriction.pdf",
                  txt_name_5 = "plot_ipw_itt_km_composite_antit_restriction.pdf")
#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))
rm(cr_data_main_antit_restriction)
#------------------------------------------------------------------------------
# use of model 2 for the per protocol analysis
#outcome
outcome_composite <- 1

#missing data
studypop_cat_missing <- 1

#causal estimand
per_protocol <- 1
per_protocol_model2 <- 1

#confounders
confounder_full <- 1

#truncation of IPW
trunc <- 0

#time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main, n=200), 
                                  input_data =  cr_data_main,
                                  location_name = "outputs/composite/sensitivity/pp",
                                  txt_name_1 = "results_ipw_pp_rr_composite_model2",
                                  txt_name_2 = "results_ipw_pp_hr_composite_5y_model2",
                                  txt_name_3 = "PP composite_model2",
                                  txt_name_4 = "plot_ipw_itt_composite_model2.pdf",
                                  txt_name_5 = "Not_required")

mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

#------------------------------------------------------------------------------
# use of model 1 for the per protocol analysis with a longer grace period

load("W:/C6_Berglund/data/procdata/angiotensin/new_swedeheart/cr_data_main_longer_grace.Rda") #load dataset

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

#time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main_longer_grace), 
                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main_longer_grace, n=200), 
                  input_data =  cr_data_main_longer_grace,
                  location_name = "outputs/composite/sensitivity/pp",
                  txt_name_1 = "results_ipw_pp_rr_composite_model1_longer_grace",
                  txt_name_2 = "results_ipw_pp_hr_composite_5y_model1_longer_grace",
                  txt_name_3 = "PP composite_model1_longer_grace",
                  txt_name_4 = "plot_ipw_itt_composite_model1_longer_grace.pdf",
                  txt_name_5 = "Not_required")

mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

#-------------------------------------------------------------------------------
# move hf diagnosis 6 months earlier, and make the diagnosis time 1 for those that now have a 
# diagnosis time that is 0 or less

cr_data_main_hf <- cr_data_main %>% 
  mutate(hf_time = hf_time-6) %>% 
  mutate(hf_time = ifelse(hf_time<=0 & !is.na(hf_time), 1, hf_time))

#outcome
outcome_heartfailure <- 1

#missing data
studypop_cat_missing <-  1

#causal estimand
per_protocol <- 1
per_protocol_model1 <- 1

#confounders
confounder_full <- 1

#truncation of IPW
trunc <- 0

#time of results
time_results <- c(12,24,36,48,60)

#choose confounders, amended to not have infarct type
source(here("scripts/analysis", "01_an_confounders.R"))

func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main_hf), 
                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main_hf, n=200), 
                  input_data =  cr_data_main_hf,
                  location_name = "outputs/hf/sensitivity",
                  txt_name_1 = "results_ipw_pp_rr_hf_diagearlier",
                  txt_name_2 = "results_ipw_pp_hr_hf_diagearlier",
                  txt_name_3 = "pp composite_hf_diagearlier",
                  txt_name_4 = "plot_ipw_pp_composite_hf_diagearlier.pdf",
                  txt_name_5 = "not_required.pdf")
#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

rm(cr_data_main_hf)

#-------------------------------------------------------------------------------
# minimally adjusted composite model

#outcome
outcome_composite <- 1

#missing data
studypop_cat_missing <- 1

#causal estimand
itt <- 1

#confounders
confounder_full <- 0

#truncation of IPW
trunc <- 0

# time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

#run code

composite_itt <- func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main), 
                                   bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main, n=200), 
                                   input_data =  cr_data_main,
                                   location_name = "outputs/composite/sensitivity/minimally_adjusted",
                                   txt_name_1 = "results_ipw_itt_rr_composite_minimally_adjusted",
                                   txt_name_2 = "results_ipw_itt_hr_composite_5y_minimally_adjusted",
                                   txt_name_3 = "Composite_minimally_adjusted",
                                   txt_name_4 = "plot_ipw_itt_composite_minimally_adjusted.pdf",
                                   txt_name_5 = "plot_ipw_itt_km_composite_minimally_adjusted.pdf")

#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))
#-------------------------------------------------------------------------------
# standardisation

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

#time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

func_fullresults (estimates = func_model_standardise (input_data =  cr_data_main), 
                                  bootstraps_estimates= func_bootstrap_standardise (input_data =  cr_data_main, n=200), 
                                  location_name = "outputs/composite/sensitivity/standardisation",
                                  input_data =  cr_data_main,
                                  txt_name_1 = "results_standardisation_itt_composite_5y.txt" , 
                                  txt_name_2 = "results_standardisation_itt_hr_composite_5y",
                                  txt_name_3 = "ITT composite_standardisation",
                                  txt_name_4 = "plot_ipw_standardisation_composite.pdf",
                                  txt_name_5 = "plot_ipw_standardisation_km_composite.pdf")

mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

#-------------------------------------------------------------------------------
# subgroup stratification for the ITT effect

#male only
cr_data_main_male <- cr_data_main %>% 
  filter(d_gender=="1")

#outcome
outcome_composite <- 1

#missing data
studypop_cat_missing <-  1

#causal estimand
itt <- 1

#confounders
confounder_gender <- 1

#truncation of IPW
trunc <- 0

# time of results
time_results <- c(12,24,36,48,60)

#choose confounders
source(here("scripts/analysis", "01_an_confounders_subgroup.R"))

func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main_male), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main_male, n=200), 
                                  input_data =  cr_data_main_male,
                                  location_name = "outputs/composite/sensitivity/gender",
                                  txt_name_1 = "results_ipw_itt_rr_composite_male",
                                  txt_name_2 = "results_ipw_itt_hr_composite_5y_male",
                                  txt_name_3 = "ITT composite_male",
                                  txt_name_4 = "plot_ipw_itt_composite_male.pdf",
                                  txt_name_5 = "plot_ipw_itt_km_composite_male.pdf")
#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

rm(cr_data_main_male)

#-------------------------------------------------------------------------------
# subgroup stratification for the ITT effect

#female only
cr_data_main_female <- cr_data_main %>% 
  filter(d_gender=="2")

#outcome
outcome_composite <- 1

#missing data
studypop_cat_missing <-  1

#causal estimand
itt <- 1

#confounders
confounder_gender <- 1

#truncation of IPW
trunc <- 0

#time of results
time_results <- c(12,24,36,48,60)

#choose confounders (amended to not have gender)
source(here("scripts/analysis", "01_an_confounders_subgroup.R"))

func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main_female), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main_female, n=200), 
                                  input_data =  cr_data_main_female,
                                  location_name = "outputs/composite/sensitivity/gender",
                                  txt_name_1 = "results_ipw_itt_rr_composite_female",
                                  txt_name_2 = "results_ipw_itt_hr_composite_5y_female",
                                  txt_name_3 = "ITT composite_female",
                                  txt_name_4 = "plot_ipw_itt_composite_female.pdf",
                                  txt_name_5 = "plot_ipw_itt_km_composite_female.pdf")
#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

rm(cr_data_main_female)


#-------------------------------------------------------------------------------
# subgroup stratification for the ITT effect

# stemi
cr_data_main_stemi <- cr_data_main %>% 
  filter(infarcttype=="1")

#outcome
outcome_composite <- 1

#missing data
studypop_cat_missing <-  1

#causal estimand
itt <- 1

#confounders
confounder_stemi <- 1

#truncation of IPW
trunc <- 0

# time of results
time_results <- c(12,24,36,48,60)

#choose confounders, amended to not have infarct type
source(here("scripts/analysis", "01_an_confounders_subgroup.R"))

func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main_stemi), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main_stemi, n=200), 
                                  input_data =  cr_data_main_stemi,
                                  location_name = "outputs/composite/sensitivity/stemi",
                                  txt_name_1 = "results_ipw_itt_rr_composite_stemi",
                                  txt_name_2 = "results_ipw_itt_hr_composite_5y_stemi",
                                  txt_name_3 = "ITT composite_stemi",
                                  txt_name_4 = "plot_ipw_itt_composite_stemi.pdf",
                                  txt_name_5 = "plot_ipw_itt_km_composite_stemi.pdf")
#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

rm(cr_data_main_stemi)

#-------------------------------------------------------------------------------
# subgroup stratification for the ITT effect

# stemi
cr_data_main_nstemi <- cr_data_main %>% 
  filter(infarcttype=="2")

#outcome
outcome_composite <- 1

#missing data
studypop_cat_missing <-  1

#causal estimand
itt <- 1

#confounders
confounder_stemi <- 1

#truncation of IPW
trunc <- 0

# time of results
time_results <- c(12,24,36,48,60)

#choose confounders, amended to not have infarct type
source(here("scripts/analysis", "01_an_confounders_subgroup.R"))

func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main_nstemi), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main_nstemi, n=200), 
                                  input_data =  cr_data_main_nstemi,
                                  location_name = "outputs/composite/sensitivity/stemi",
                                  txt_name_1 = "results_ipw_itt_rr_composite_nstemi",
                                  txt_name_2 = "results_ipw_itt_hr_composite_5y_nstemi",
                                  txt_name_3 = "ITT composite_nstemi",
                                  txt_name_4 = "plot_ipw_itt_composite_nstemi.pdf",
                                  txt_name_5 = "plot_ipw_itt_km_composite_nstemi.pdf")
#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

rm(cr_data_main_nstemi)

#-------------------------------------------------------------------------------
# subgroup stratification for the ITT effect

#age under 60
cr_data_main_ageyounger <- cr_data_main %>% 
  filter(d_age_hia<60)

#outcome
outcome_composite <- 1

#missing data
studypop_cat_missing <-  1

#causal estimand
itt <- 1

#confounders
confounder_age <- 1

#truncation of IPW
trunc <- 0

#time of results
time_results <- c(12,24,36,48,60)

#choose confounders, amended to not have infarct type
source(here("scripts/analysis", "01_an_confounders.R"))

func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main_ageyounger), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main_ageyounger, n=200), 
                                  input_data =  cr_data_main_ageyounger,
                                  location_name = "outputs/composite/sensitivity/age",
                                  txt_name_1 = "results_ipw_itt_rr_composite_ageyounger",
                                  txt_name_2 = "results_ipw_itt_hr_composite_5y_ageyounger",
                                  txt_name_3 = "ITT composite_ageyounger",
                                  txt_name_4 = "plot_ipw_itt_composite_ageyounger.pdf",
                                  txt_name_5 = "plot_ipw_itt_km_composite_ageyounger.pdf")
#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

rm(cr_data_main_ageyounger)

#-------------------------------------------------------------------------------
# subgroup stratification for the ITT effect

#age 60 or above
cr_data_main_ageolder <- cr_data_main %>% 
  filter(d_age_hia>=60)

#outcome
outcome_composite <- 1

#missing data
studypop_cat_missing <-  1

#causal estimand
itt <- 1

#confounders
confounder_age <- 1

#truncation of IPW
trunc <- 0

#time of results
time_results <- c(12,24,36,48,60)

#choose confounders, amended to not have infarct type
source(here("scripts/analysis", "01_an_confounders.R"))

func_fullresults (estimates = func_model_ipw (input_data =  cr_data_main_ageolder), 
                                  bootstraps_estimates= func_bootstrap_ipw (input_data =  cr_data_main_ageolder, n=200), 
                                  input_data =  cr_data_main_ageolder,
                                  location_name = "outputs/composite/sensitivity/age",
                                  txt_name_1 = "results_ipw_itt_rr_composite_ageolder",
                                  txt_name_2 = "results_ipw_itt_hr_composite_5y_ageolder",
                                  txt_name_3 = "ITT composite_ageolder",
                                  txt_name_4 = "plot_ipw_itt_composite_ageolder.pdf",
                                  txt_name_5 = "plot_ipw_itt_km_composite_ageolder.pdf")
#reset decisions
mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

rm(cr_data_main_ageolder)

