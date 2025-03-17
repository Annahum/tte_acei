# R-PROFILE --------------------------------------------------------------------
source("W:/C6_Berglund/ahumphreys/r-profile.R")

# PACKAGES ---------------------------------------------------------------------
library(here)
library(dplyr)
options(dplyr.summarise.inform = FALSE)
library(purrr)
library(tableone)
library(splitstackshape)
library(tidyr)
library(ggplot2)
library(survival)
library(survminer)
library(survey)
library(tictoc)
library(pammtools)
library(stringr)
library(gridExtra)
library(WeightIt)
library(cobalt)
library(splines)
#library(rlang)
library(labelled)
library(berryFunctions)
library(ggsci)
library(rsample)
library(lobstr)
library(rms)
library(furrr)
library(future.apply)
library(parallel)
library(data.table)
library(gridExtra)
library(grid)
library(patchwork)
library(ggtext)


# LOAD MAIN FUNCTIONS ---------------------------------------------------------------

source(here("scripts/analysis", "func_ipweights.R"))
source(here("scripts/analysis", "func_censor.R"))
source(here("scripts/analysis", "func_persontime.R"))
source(here("scripts/analysis", "func_analyticdecision.R"))
source(here("scripts/analysis", "func_model_ipw.R"))
source(here("scripts/analysis", "func_ipw_adherence.R"))
source(here("scripts/analysis", "func_model_standardise.R"))
source(here("scripts/analysis", "func_bootstrap.R"))
source(here("scripts/analysis", "func_fullresults.R"))

#Set decisions to 0

decisions <- c("outcome_composite", "outcome_heartfailure", "outcome_death", "outcome_mi", "outcome_cancer",
               "studypop_median", "studypop_completecase", "studypop_cat_missing", "itt", "per_protocol",
               "per_protocol_model1", "per_protocol_model2", "confounder_full", "trunc",  "botharms", "time_results",
               "confounder_pci", "confounder_gender", "confounder_stemi", "confounder_age")

mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))

# LOAD MAIN DATA ---------------------------------------------------------------
load("W:/C6_Berglund/data/procdata/angiotensin/new_swedeheart/cr_data_main.Rda")

# DATA ANALYSIS ----------------------------------------------------------------

# 1 - Identify confounders for analyses
source(here("scripts/analysis", "01_an_confounders.R"))
source(here("scripts/analysis", "01_an_confounders_subgroup.R"))

# 2 - Make table one
source(here("scripts/analysis", "03_tableone.R"))

# 3 - Run analyses
source(here("scripts/analysis", "02_analysis_ipw.R"))
source(here("scripts/analysis", "02_analysis_sensitivity_subgroup.R"))

# 4 - Make forest plots, and other composite plots
source(here("scripts/analysis", "04_plots.R"))