# analysis decisions

func_analyticdecision <- function(input_data) {
  
  cr_data_main <- input_data
  
#-------------------------------------------------------------------------------
if (outcome_composite) {
  
  cr_data_main <- cr_data_main %>% 
    mutate(contraind_time = pmin(ras_tu, hypotension_tu, na.rm = TRUE)) %>% 
    mutate(ind_time = hypertension_tu)  %>% 
    mutate(outcome_time= composite_time) %>% 
    select(-c(cancer_time, hf_time, death_time, composite_time, mi_time, hypotension_tu, ras_tu, hf_tu, hypertension_tu))
  }

#-------------------------------------------------------------------------------
if (outcome_heartfailure) {
  
cr_data_main <- cr_data_main %>% 
  mutate(contraind_time = pmin(ras_tu, hypotension_tu, na.rm = TRUE)) %>% 
  mutate(ind_time = hypertension_tu) %>% 
  mutate(outcome_time= hf_time) %>% 
  select(-c(cancer_time, hf_time, death_time, composite_time, mi_time, hypotension_tu, ras_tu, hf_tu, hypertension_tu))
}

#-------------------------------------------------------------------------------
if (outcome_death) {
cr_data_main <- cr_data_main  %>% 
  mutate(contraind_time = pmin(ras_tu, hypotension_tu, na.rm = TRUE)) %>% 
  mutate(ind_time = pmin(hypertension_tu, hf_tu, na.rm = TRUE)) %>%  #inclusion of heart failure as indication 
  mutate(outcome_time= death_time) %>% 
  select(-c(cancer_time, hf_time, death_time, composite_time, mi_time,hypotension_tu, ras_tu, hf_tu, hypertension_tu))
}

#-------------------------------------------------------------------------------
if (outcome_mi) {
  cr_data_main <- cr_data_main  %>% 
    mutate(contraind_time = pmin(ras_tu, hypotension_tu, na.rm = TRUE)) %>% 
    mutate(ind_time = pmin(hypertension_tu, hf_tu, na.rm = TRUE)) %>%  #inclusion of heart failure as indication 
    mutate(outcome_time= mi_time) %>% 
    select(-c(cancer_time, hf_time, death_time, composite_time, mi_time, hypotension_tu, ras_tu, hf_tu, hypertension_tu))
}

#-------------------------------------------------------------------------------
if (outcome_cancer) {
  cr_data_main <- cr_data_main  %>% 
    mutate(contraind_time = pmin(ras_tu, hypotension_tu, na.rm = TRUE)) %>% 
    mutate(ind_time = pmin(hypertension_tu, hf_tu, na.rm = TRUE)) %>%  #inclusion of heart failure as indication 
    mutate(outcome_time= cancer_time) %>% 
    select(-c(cancer_time, hf_time, death_time, composite_time, mi_time, hypotension_tu, ras_tu, hf_tu, hypertension_tu))
}


#-------------------------------------------------------------------------------

#different decisions re missing data

#import median values of continuous variables for missing
#no missing age
if (studypop_median) {
  
  func_impute_median <- function(data_input, varlist_numeric) {
    varlist_numeric <- c("d_age_hia", "dispink04", "d_bmi", "d_ldl_cholesterol", "d_hdl_cholesterol", "egfr", "diastolic_blood_pressure",
                         "systolic_blood_pressure", "d_bmi", "heart_rate") 
    for (var in varlist_numeric) {
      if (var %in% names(data_input)) {
        data_input[[var]][is.na(data_input[[var]])] <- median(data_input[[var]], na.rm = TRUE)
      } else {
        warning(paste("Variable", var, "is not in the data frame and will be skipped."))
      }
    }
    return(data_input)
  }
  
  cr_data_main <-  func_impute_median (data_input = cr_data_main)
  
  }


#-------------------------------------------------------------------------------

#make complete case analysis datasets  (identify first the numerical variables)
if (studypop_completecase) {
  
func_completecase <- function(data_input, varlist) {
    
    varlist_num <- c("d_age_hia", "heart_rate", "systolic_blood_pressure", "diastolic_blood_pressure",
                     "d_ldl_cholesterol", "d_hdl_cholesterol", "egfr", "d_bmi")
    
    # get numeric (without id) and factor variable names
    varlist_factor <- names(select(data_input[varlist], is.factor)) 
    # drop missing  9 for factor
    data <- data_input %>% 
      filter_at(vars(varlist_factor), all_vars(.!=9)) %>% 
      filter_at(vars(varlist_num), all_vars(!is.na(.))) 
    
    return(data[varlist])
    
  }

cr_data_main <- func_completecase(data_input = cr_data_main) 

 
}

#-------------------------------------------------------------------------------
#categorise continuous variables and make missing indicator
if (studypop_cat_missing) {
  
  # create the factor levels
  cr_data_main <- cr_data_main %>% 
  mutate(heart_rate = cut(heart_rate, breaks = c(0,59,100,999)),
         dispink04 = cut(dispink04, breaks = c(-30000, 1000, 2000, 3000, 4000,  999999)),
         d_bmi = cut(d_bmi, breaks = c(0, 18.4, 24.9, 29.9, 999)),
         d_hdl_cholesterol = cut(d_hdl_cholesterol, breaks = c(0, 0.9, 1.5, 999)), 
         d_ldl_cholesterol = cut(d_ldl_cholesterol, breaks = c(0, 3.3, 4.8, 999)),       
         systolic_blood_pressure = cut(systolic_blood_pressure, breaks = c(0,119,139,999)),
         diastolic_blood_pressure = cut(diastolic_blood_pressure, breaks = c(0,79,89,999)),
         egfr = cut(egfr, breaks = c(0,59,89,119, 999)) #updated
  ) %>% 
    # make NA a factor level 
    mutate(across(c( heart_rate, dispink04, d_bmi, d_hdl_cholesterol, d_ldl_cholesterol, systolic_blood_pressure, diastolic_blood_pressure,
                      egfr), 
                  ~addNA(.)))
  
}
#------------------------------------------------------------------------------
if (per_protocol_model1) {

cr_data_main <- cr_data_main %>% 
  select( -c(notreatment_duration.model2, treatment_duration.model2)) %>% 
  rename(treatment_duration = treatment_duration.model1) %>% 
  rename(notreatment_duration = notreatment_duration.model1)
}

#------------------------------------------------------------------------------
if (per_protocol_model2) {
  
  cr_data_main <- cr_data_main %>% 
    select( -c(notreatment_duration.model1, treatment_duration.model1)) %>% 
    rename(treatment_duration = treatment_duration.model2) %>% 
    rename(notreatment_duration = notreatment_duration.model2)
}
  
  return(cr_data_main)
}