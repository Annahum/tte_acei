#identify max follow up time (either outcome, end of follow up, or administrative end)
#then identify treatment duration for measuring per protocol

func_censor <- function(input_data) {

cr_data_main_temp <- input_data %>%
  mutate(lossfup_date_min = pmin(maxfup_time, endfup_time, na.rm = TRUE)) %>% 
  mutate(maxfup_new =pmin(outcome_time, lossfup_date_min, na.rm = TRUE))  

if (itt) {

  cr_data_treatmentlength <-  cr_data_main_temp %>% 
    mutate(followup_duration = maxfup_new)
  
  return(cr_data_treatmentlength)
  
}

if (per_protocol) {
#treatment length calculation for per protocol
# For individuals on treatment (itt=1) who had a contraindication before the maximum treatment duration, they are not censored

cr_data_main_temp1 <- cr_data_main_temp %>%
  filter(!is.na(contraind_time), contraind_time <= treatment_duration, intervention==1) %>%
  mutate(censor_nonadherence_time = NA,
         censor_reason = "itt1_not_censored_had_contraind",
         followup_duration = maxfup_new) %>%
  select(id, censor_nonadherence_time, censor_reason, followup_duration)  

# For individuals on treatment (itt=1) who had no contraindication, censoring should occur when they stop adhering

cr_data_main_temp2 <- cr_data_main_temp %>%
  filter(intervention == 1) %>% 
  anti_join(cr_data_main_temp1, by = "id") %>% 
  mutate(censor_nonadherence_time = treatment_duration,
         censor_reason = "itt1_censored",
         followup_duration = maxfup_new) %>%
  select(id, censor_nonadherence_time, censor_reason, followup_duration)  

#----------------------------------------------------------------------------------------------
# For individuals not on treatment (itt=0) who had another indication and then started taking treatment, they are not censored
cr_data_main_temp3 <- cr_data_main_temp %>%
  filter(!is.na(ind_time), ind_time <= notreatment_duration, intervention==0) %>%
  mutate(censor_nonadherence_time = NA,
         censor_reason = "itt0_not_censored_had_indication",
         followup_duration = maxfup_new) %>% 
  select(id, censor_nonadherence_time, censor_reason, followup_duration)  

# For individuals not on treatment (itt=0) who had no indication , censoring should occur when they stop treatment

cr_data_main_temp4 <- cr_data_main_temp %>%
  filter(intervention == 0) %>% 
  anti_join(cr_data_main_temp3, by = "id") %>% 
  mutate(censor_nonadherence_time = notreatment_duration,
       censor_reason = "itt0_censored",
       followup_duration = maxfup_new) %>%
  select(id, censor_nonadherence_time, censor_reason, followup_duration)  

#----------------------------------------------------------------------------------------
  
cr_treatmentlength <- rbind(cr_data_main_temp1, cr_data_main_temp2, cr_data_main_temp3, cr_data_main_temp4)

cr_data_treatmentlength <- cr_data_main_temp %>% 
  left_join(cr_treatmentlength, by = "id") %>% 
  # Create time that each person exits due to event, lossfup or non_adherence
  rowwise() %>% 
  mutate(exit_date = min(c(followup_duration, censor_nonadherence_time), na.rm = TRUE)) %>%
  ungroup()  %>% 
  select(-c( treatment_duration, notreatment_duration,followup_duration,  ind_time, maxfup_time, endfup_time, contraind_time, censor_reason)) 

rm(cr_data_main_temp1, cr_data_main_temp2, cr_data_main_temp3, cr_data_main_temp4,  cr_data_main_temp, cr_treatmentlength)

return(cr_data_treatmentlength)
}

}