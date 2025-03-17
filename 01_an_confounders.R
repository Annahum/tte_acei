#-------------------------------------------------------------------------------

# Categorisation of continuous variables with missing data -
if (studypop_cat_missing) {
  
  if(confounder_full) {
    
    confounders_baseline <- c(
      
      "centre_name",
      "eligible_year",
      
      # Characteristics and prior diagnoses at baseline
      "ns(d_age_hia,5)", #no missing data
      "d_gender",
      "smoking_status",
      "previous_mi",
      "previous_stroke",
      "previous_pci",
      "prior_cardiac_surgery",
      
      #ses
      "d_civil",
      "birthcountry",
      "dispink04",
      "sun2000niva_n",
      "occupation_status",
      
      # Presentation
      "infarcttype",
      "cpr_before_hospital",
      "thromb_before_hospital",
      "cardiac_shock",
      "ecg_rhythm",
      "ecg_qrs_annotation",
      "ecg_stt_changes",
      
      # In hospital
      "d_pci",
      "fynd",
      "stenosklass",
      "segment",
      "d_beta_blockers_treat",
      "d_diuretics_treat",
      "d_inotropes",
      "d_nitrates_treat",
      
      # Readings
      "heart_rate",
      "systolic_blood_pressure",
      "diastolic_blood_pressure",
      "d_ldl_cholesterol",
      "d_hdl_cholesterol",
      "egfr",
      "d_bmi",
      
      # Previous medications at admission or in time zero
      "bb_baseline",
      "diabetes_treatment_baseline",
      "ccb_baseline",
      "diuretics_baseline",
      "nitrates_baseline" 
    ) }
  
  else {
    
    confounders_baseline <- c(
      "ns(d_age_hia,5)",
      "d_gender") }
  
  if (per_protocol) {
    
    confounders_timeupdated <- c(
      
      "centre_name",
      "eligible_year",
      
      # Characteristics and prior diagnoses at baseline
      "ns(d_age_hia,5)", #no missing data
      "d_gender",
      "smoking_status",
      "previous_mi",
      "previous_stroke",
      "previous_pci",
      "prior_cardiac_surgery",
      
      #ses
      "d_civil",
      "birthcountry",
      "dispink04",
      "sun2000niva_n",
      "occupation_status",
      
      # Presentation
      "infarcttype",
      "cpr_before_hospital",
      "thromb_before_hospital",
      "cardiac_shock",
      "ecg_rhythm",
      "ecg_qrs_annotation",
      "ecg_stt_changes",
      
      # In hospital
      "d_pci",
      "fynd",
      "stenosklass",
      "segment",
      "d_beta_blockers_treat",
      "d_diuretics_treat",
      "d_inotropes",
      "d_nitrates_treat",
      
      # Readings
      "heart_rate",
      "systolic_blood_pressure",
      "diastolic_blood_pressure",
      "d_ldl_cholesterol",
      "d_hdl_cholesterol",
      "egfr",
      "d_bmi",
      
      #time updated drugs (currently numeric)
      "bb_tu",
      "diabetes_treatment_tu",
      "ccb_tu",
      "diuretics_tu",
      "nitrates_tu",
      
      #time updated comorbs
      "renal_tu",
      "diabetes_tu"
      
    )
  }
}


# Median -------------------------------------------------------------------------
if (studypop_median) {  #continuous variables with missing data kept continuous
  
  if (confounder_full) {
  
  confounders_baseline <- c(
  
  "centre_name",
  "eligible_year",

  # Characteristics and prior diagnoses at baseline
  "ns(d_age_hia,5)",
  "d_gender",
  "smoking_status",

  "previous_mi",
  "previous_stroke",
  "previous_pci",
  "prior_cardiac_surgery",

  #ses
  "d_civil",
  "birthcountry",
  "dispink04",
  "sun2000niva_n",
  "occupation_status",

  # Presentation
  "infarcttype",
  "cpr_before_hospital",
  "thromb_before_hospital",
  "cardiac_shock",
  "ecg_rhythm",
  "ecg_qrs_annotation",
  "ecg_stt_changes",
  
  # In hospital
  "d_pci",
  "fynd",
  "stenosklass",
  "segment",
  "d_beta_blockers_treat",
  "d_diuretics_treat",
  "d_inotropes",
  "d_nitrates_treat",
  
  # Readings
  "ns(heart_rate,5)",
  "ns(systolic_blood_pressure,5)",
  "ns(diastolic_blood_pressure,5)",
  "ns(d_ldl_cholesterol,5)",
  "ns(d_hdl_cholesterol,5)",
  "ns(egfr,5)",
  "ns(d_bmi,5)",
  
  # Previous medications at admission or in time zero
  "bb_baseline",
  "diabetes_treatment_baseline",
  "ccb_baseline",
  "diuretics_baseline",
  "nitrates_baseline" 
)

  } else {
    
    confounders_baseline <- c(
      "ns(d_age_hia,5)",
      "d_gender") }

}


# Complete case -------------------------------------------------------------------------
if (studypop_completecase) {  #continuous variables with missing data kept continuous - 
  #note d_pci has to be removed from the model as everyone non missing data has had pci
  
  if (confounder_full) {
    
    confounders_baseline <- c(
      
      "centre_name",
      "eligible_year",
      
      # Characteristics and prior diagnoses at baseline
      "ns(d_age_hia,5)",
      "d_gender",
      "smoking_status",
      
      "previous_mi",
      "previous_stroke",
      "previous_pci",
      "prior_cardiac_surgery",
      
      #ses
      "d_civil",
      "birthcountry",
      "dispink04",
      "sun2000niva_n",
      "occupation_status",
      
      # Presentation
      "infarcttype",
      "cpr_before_hospital",
      "thromb_before_hospital",
      "cardiac_shock",
      "ecg_rhythm",
      "ecg_qrs_annotation",
      "ecg_stt_changes",
      
      # In hospital
      "fynd",
      "stenosklass",
      "segment",
      "d_beta_blockers_treat",
      "d_diuretics_treat",
      "d_inotropes",
      "d_nitrates_treat",
      
      # Readings
      "ns(heart_rate,5)",
      "ns(systolic_blood_pressure,5)",
      "ns(diastolic_blood_pressure,5)",
      "ns(d_ldl_cholesterol,5)",
      "ns(d_hdl_cholesterol,5)",
      "ns(egfr,5)",
      "ns(d_bmi,5)",
      
      # Previous medications at admission or in time zero
      "bb_baseline",
      "diabetes_treatment_baseline",
      "ccb_baseline",
      "diuretics_baseline",
      "nitrates_baseline" 
    )
    
  } else {
    
    confounders_baseline <- c(
      "ns(d_age_hia,5)",
      "d_gender") }
  
}
