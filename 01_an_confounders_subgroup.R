if (studypop_cat_missing) {  
  
if (confounder_gender) {  
    
    confounders_baseline <- c(
      
      "centre_name",
      "eligible_year",
      
      # Characteristics and prior diagnoses at baseline (gender removed)
      "ns(d_age_hia,5)", #no missing data
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
    )
    
  } 
  
  if (confounder_age) {  
    
    confounders_baseline <- c(
      
      "centre_name",
      "eligible_year",
      
      # Characteristics and prior diagnoses at baseline
      "ns(d_age_hia,3)", #changee to 3 splines
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
    )
    
  } 
  
  if (confounder_stemi) {  
    
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
      
      #presentation (due to bootstrap modeling issues - removal of infarcttype,ecg_qrs_annotation, ecg_stt_changes )
      "cpr_before_hospital",
      "thromb_before_hospital",
      "cardiac_shock",
      "ecg_rhythm",

      # In hospital (due to bootstrap modeling issues - removal of fynd)
      "d_pci",
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
    )
    
  }
}