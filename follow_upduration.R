
# itt

load("W:/C6_Berglund/data/procdata/angiotensin/new_swedeheart/cr_data_main.Rda")

#outcome
outcome_composite <- 1

#missing data
studypop_cat_missing <- 1

#causal estimand
itt <- 1

#get data ready
input_data_prep <- func_analyticdecision (input_data = cr_data_main)

#calculate adherence and follow up duration
cr_followuplength <- func_censor (input_data = input_data_prep ) %>% 
  select(id, intervention, outcome_time, followup_duration)

#median follow up duration aceI group
summary(cr_followuplength$followup_duration[cr_followuplength$intervention==1])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.0    47.0    60.0    51.8    60.0    60.0 

#median follow up duration no aceI group
summary(cr_followuplength$followup_duration[cr_followuplength$intervention==0])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00   45.00   60.00   51.03   60.00   60.00 

#overall median follow up
summary(cr_followuplength$followup_duration)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00   46.00   60.00   51.56   60.00   60.00 

mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))
#-------------------------------------------------------------------------------

# per protocol

#outcome
outcome_composite <- 1

#missing data
studypop_cat_missing <- 1

#causal estimand
per_protocol <- 1
per_protocol_model1 <- 1

#get data ready
input_data_prep <- func_analyticdecision (input_data = cr_data_main)

#calculate adherence and follow up duration
cr_data_treatmentlength <- func_censor (input_data = input_data_prep )

cr_followuplength <- cr_data_treatmentlength %>% 
  select(id, intervention, censor_nonadherence_time, exit_date ) %>%  #exit date is due to admin end, event, or non adherence
  mutate(ppcensor_fixed = case_when(
    censor_nonadherence_time == exit_date ~ 1L,
    is.na(censor_nonadherence_time) ~ 0L, 
    TRUE ~ 0L
  )) 
  
#median follow up duration aceI group
summary(cr_followuplength$exit_date[cr_followuplength$intervention==1])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0    19.0    45.0    39.6    60.0    60.0 

#median follow up other group
summary(cr_followuplength$exit_date[cr_followuplength$intervention==0])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   26.00   58.00   43.13   60.00   60.00 
 
#adherence time 
table(cr_followuplength$intervention, cr_followuplength$ppcensor_fixed)

#                  0    1
# Intervention 0 3740 990
# Intervention 1 4873 5824

#make unweighted plot
cr_data_long <- func_persontime(input_data = cr_data_treatmentlength )

km_dat <- cr_data_long  %>% 
  group_by(id) %>% 
  select(id, ppcensor, time, intervention, surv) %>% 
  mutate(censor_max = max(ppcensor)) %>% 
  ungroup() %>% 
  filter(time==0) %>% 
  mutate(censor_max = ifelse(is.na(censor_max), 0, censor_max)) 

survdiff(Surv(surv, censor_max) ~ intervention, data=km_dat)

fit <- survfit(Surv(surv, censor_max) ~ intervention, 
                      data=km_dat)

ipw_plot_adherence <-  ggsurvplot(fit, 
             data = km_dat, 
             fun = function(x) 100*(1-x),
             xlab="months of follow-up",
             ylab="Risk (%)",
             main="Product-Limit Survival Estimates", 
             risk.table = TRUE)

mapply(assign, decisions, 0, MoreArgs = list(envir = .GlobalEnv))


# Drop all dataframes and vectors
rm(list = ls())

