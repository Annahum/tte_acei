func_persontime <- function(input_data) {
  
  if (itt) { 

  cr_data_main_long <- {{input_data}} %>% 
    
    
# create binary for outcome 
    mutate(outcome_fixed = case_when(
      outcome_time == followup_duration ~ 1L,
      is.na(outcome_time) ~ 0L, 
      TRUE ~ 0L
    )) %>% 
    
# create binary for lossfup 
    mutate(lossfup_fixed = case_when(
      lossfup_date_min == followup_duration ~ 1L,
      is.na(lossfup_date_min) ~ 0L, 
      TRUE ~ 0L
    )) %>% 
    
# Make surv variable 
    mutate(surv = followup_duration) %>% 
    
# Split period until leave
    expandRows("surv", drop=F) %>% 
    
# Create a variable that indicates the time period
    mutate(time = sequence(rle(id)$lengths)-1) %>% 
    
# Create binary variable for outcome time updated (from k+1)
    mutate(outcome = ifelse(time == surv - 1 & outcome_fixed == 1, 1, 0)) %>%
    
# Create binary variable for lossfup time updated (from k+1)
    mutate(lossfup = ifelse(time == surv - 1 & lossfup_fixed == 1, 1, 0)) %>%
    
# if lossfup, make event NA in that period
    mutate(outcome = ifelse(lossfup==1, NA, outcome)) %>% 
    
#create powers of time to allow time dependent baseline hazard
    mutate(time_2 = time^2,
           time_3 = time^3) %>%
    relocate(time, .before=time_2) %>% 
    
# remove unwanted variables
  select(-c(contains("min"), contains("date"), contains("fixed"))) 
  
}

# CREATE VECTOR OF ALL TIME UPDATED VARIABLE IN DATA----------------------------   
if (per_protocol) {     

tu <- colnames({{input_data}}) %>% str_subset(pattern = "_tu")
tu_baseline <- gsub("_tu", "_baseline", tu)

cr_data_main_long <- {{input_data}} %>%
  

# exit date - time each person exits due to event, lossfup or non_adherence

# create binary for outcome 
    mutate(outcome_fixed = case_when(
    outcome_time == exit_date ~ 1L,
      is.na(outcome_time) ~ 0L, 
      TRUE ~ 0L
    )) %>% 
    
# create binary for lossfup 
    mutate(lossfup_fixed = case_when(
      lossfup_date_min == exit_date ~ 1L,
      is.na(lossfup_date_min) ~ 0L, 
      TRUE ~ 0L
    )) %>% 
    
# Create binary for pp censor
    mutate(ppcensor_fixed = case_when(
      censor_nonadherence_time == exit_date ~ 1L,
      is.na(censor_nonadherence_time) ~ 0L, 
      TRUE ~ 0L
    )) %>% 

# Make surv variable 
    mutate(surv = exit_date) %>% 
    
# Split period until leave
    expandRows("surv", drop=F) %>% 
    
# Create a variable that indicates the time period
    mutate(time = sequence(rle(id)$lengths)-1) %>% 
    
# Create binary variable for outcome time updated (from k+1)
    mutate(outcome = ifelse(time == surv - 1 & outcome_fixed == 1, 1, 0)) %>%
    
# Create binary variable for lossfup time updated (from k+1)
    mutate(lossfup = ifelse(time == surv - 1 & lossfup_fixed == 1, 1, 0)) %>%
    
# Create binary variable for pp censoring time updated (from k+1)
    mutate(ppcensor = ifelse(time == surv - 1 & ppcensor_fixed == 1, 1, 0)) %>%
    
# if lossfup or pp censored, make event NA in that period 
    mutate(outcome = ifelse(lossfup==1, NA, outcome)) %>% 
    mutate(outcome = ifelse(ppcensor==1, NA, outcome))  %>% 
  
# kidney disease and diabetes are indications after an MI, but afterwards are treated as 
# time varying confounders (so must be not present at baseline)
    mutate(renal_baseline = 0) %>% 
    mutate(diabetes_baseline = 0) %>% 
    
# Update the time updated covariates to be binary and change during period there is an update
# Also takes the baseline variable into account
    mutate(across(.cols = tu_baseline,
                  .fns = ~ .,
                  .names = "{.col}_temp"
    )) %>% 
  rename_with(~sub("_baseline_temp", "_tu_temp", .), ends_with("_baseline_temp")) %>%
  
    mutate(across(.cols = tu,
                  .fns = ~case_when(
                    get(glue::glue("{cur_column()}_temp")) == 1 ~ 1, 
                    get(glue::glue("{cur_column()}_temp")) == 0 & . <= time ~  1,
                    get(glue::glue("{cur_column()}_temp")) == 0 & . > time ~  0
                  )
    )) %>% 
    select(-contains("tu_temp")) %>% 

    # Make timeupdated variables factors
    mutate_at(vars(tu), list(~factor(.))) %>% 
    
    #create powers of time to allow time dependent baseline hazard
    mutate(time_2 = time^2,
           time_3 = time^3) %>%
    relocate(time, .before=time_2) %>% 
    
    # remove unwanted variables
    select(-c(contains("min"), contains("date"), contains("_time"), contains("fixed"), renal_baseline, diabetes_baseline)) %>% 
    
    select(id, intervention, outcome, lossfup, ppcensor, surv, time, time_2, time_3, everything())
    
  }
  
  return(cr_data_main_long)
  
} # end function


