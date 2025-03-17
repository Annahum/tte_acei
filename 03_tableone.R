# Only baseline variables require
func_cr_tableone <- function(input_data, intervention, txt_name) {
  
  # Select columns needed
  # Remove variable that end in _tu (time updated), outcomes/indications/contraindications/ treatment length/index_date
  input_data <- input_data %>%
    select(-ends_with("_tu"), -ends_with("_time"), -contains("treatment_d"), -c(id, index_date))
 
  #assign remaining column names to names (and remove intervention)
  vars <- names(input_data)
  vars <- vars[!vars %in% c("intervention")]
  
   #identify categorical variables
  vars_cat <- names(Filter(is.factor, input_data))  
    
  # Make 9s missing for categorical variables
  func_missing <- function(x) {factor(x, exclude = 9)}
  input_data <- input_data %>% mutate_at(vars_cat, func_missing)
  
  # Label categorical variables where needed
  input_data$intervention <- factor(input_data$intervention, levels=c(1,0),
                                    labels=c("ACE I/ARB", "No ACE I/ARB"))

  input_data$centre_name<- factor(input_data$centre_name,levels=c("SE10011", "SE10013", "SE11001",
                               "SE11002", "SE11010", "SE12001", "SE13010", "SE21001", "SE21013",
                               "SE21014", "SE22010", "SE22011", "SE23011", "SE25010", "SE28010",
                               "SE28011", "SE30001", "SE41001", "SE41012", "SE42010", "SE42011",
                               "SE50001", "SE50010", "SE51011", "SE51012", "SE52011", "SE52012",
                               "SE53011", "SE53013", "SE54010", "SE55010", "SE56010", "SE56012",
                               "SE57010", "SE57011", "SE62010", "SE63010", "SE64001", "SE65016", 
                               "SE11011", "SE11012", "SE12010", "SE13011", "SE13012", "SE22012", "SE23010",
                               "SE24010", "SE25011", "SE26010", "SE27010", "SE27011", "SE28012",
                               "SE41010", "SE41011", "SE41013", "SE52017", "SE54012", "SE54014",
                               "SE55011", "SE55012", "SE57010", "SE57011", "SE57013", "SE61010",
                               "SE61011", "SE61012", "SE62010", "SE62011", "SE62013", "SE64010",
                               "SE64011", "SE65012", "SE65013", "SE65014", "other"
                               ), 
                        labels=c("Stockholm St Göran", "Stockholm SÖS", "Stockholm KI Solna",
                                "Stockholm KI Huddinge", "Stockholm Danderyd", "Uppsala",
                                "Eskiltuna", "Linköping", "Norrköping Vrinnevi",
                                "Motala", "Jönköping", "Eksjö", "Ljungby", "Kalmar", "Kristianstad",
                                "Ängelholm", "Malmö", "Lund", "Helsingborg", "Halmstad", "Varberg",
                                "Göteberg Sahlgrenska", "Göteborg Östra", "Göteborg Mölndal",
                                "Kungälv", "Borås", "Alingsås","Lidköping", "Skövde", "Karlstad",
                                "Örebro", "Västerås", "Köping", "Falun", "Mora", "Sundsvall", 
                                "Östersund", "Umeå", "Sunderbyn", "Södertälje", "Norrtälje",
                                "Enköping", "Nyköping", "Katrineholm", "Värnamo", "Växjö", "Västervik", "Oskarshamn",
                                "Visby", "Karlskrona", "Karlshamn", "Hässleholm", "Landskrona lasarett",
                                "Trelleborg", "Ystad", "Trollhättan", "Arvika", "Torsby", "Karlskoga",
                                "Lindesberg", "Falun", "Mora", "Avesta", "Gävle", "Bollnäs", "Hudiksvall", 
                                "Sundsvall", "Örnsköldsvik", "Sollefteå", "Skellefteå", "Lycksele", "Gällivare",
                                "Piteå", "Kalix", "Other" ))
                                

  input_data$smoking_status <- factor(input_data$smoking_status, levels=c(0,1,2),
  labels=c("Never smoker", "Ex-smoker (>1 month)", "Smoker"))
  
  input_data$infarcttype <- factor(input_data$infarcttype, levels=c(1,2),
                                  labels=c( "STEMI", "NSTEMI"))
  
  
  input_data$ecg_rhythm <- factor(input_data$ecg_rhythm, levels=c(1,2,8),
  labels=c("Sinus", "Ability flicker/flutter", "Other"))
  
  input_data$ecg_qrs_annotation <- factor(input_data$ecg_qrs_annotation, levels=c(1,2,3,4,5,8),
  labels=c("Normal", "Pacemaker", "Left block branch", "Pathological Q-wave", "Right block branch", "Other"))
  
  input_data$ecg_stt_changes <- factor(input_data$ecg_stt_changes, levels=c(1,2,3,4,8),
  labels=c("Normal", "ST elevation", "ST depression", "Pathological T-wave", "Other"))
  
  input_data$fynd <- factor(input_data$fynd, levels=c(1, 2,3,4,5),
  labels=c("Normal", "One vessel", "Two vessels", "Three vessels",
           "Left main"))
  
  input_data$segment<- factor(input_data$segment, levels=c(1,2,3,4,5,6),
                              labels=c("0%", "<50% ", "50-69%", "70-89%", "90-99%","100%"))

  input_data$birthcountry <- factor(input_data$birthcountry, levels=c(1,2,3,4),
                                    labels=c("Sweden", "Other Nordic countries", "Europe excluding Nordics", "Outside of Europe"))

  input_data$occupation_status <- factor(input_data$occupation_status, levels=c(1,2,3,4, 5),
                                         labels=c("Working", "Sick Leave", "Unemployed", "Retired", "Student"))
  
  input_data$sun2000niva_n <- factor(input_data$sun2000niva_n, levels=c(1,2,3,4,5,6),
                                     labels=c("Pre-secondary education less than 9 years old", "Pre-secondary education of 9 years (equivalent)",
                                              "Secondary education up to 2 years", "Secondary education of 3 years", "Post-secondary education less than 3 years old",
                                              "Post-secondary education 3 years or more and postgraduate education"))
  
  input_data$d_civil <- factor(input_data$d_civil, levels=c(1,2,3,4),
                               labels=c("Married/ partner","Unmarried/ not partnered", "Divorced", "Widow/ widower/ surviving partner"))
  
tableone_temp <- CreateTableOne(vars = vars,
                               strata = {{intervention}},
                               includeNA = FALSE,
                               data = input_data)
    

  tableoneprint <- print(tableone_temp,
                         nonnormal = TRUE,
                         contDigits = 1,
                         test = FALSE,
                         missing = TRUE,
                         quote = FALSE,
                         varLabel = FALSE,
                         dropEqual = TRUE,
                         noSpace = TRUE,
                         smd=TRUE)
  tableoneprint
 
# Save to output file
  write.table(tableoneprint, here("outputs",paste0(txt_name, ".txt")), sep="\t", quote = FALSE, row.names=TRUE, col.names=NA)
}

table1 <- func_cr_tableone(input_data = cr_data_main, intervention = "intervention", txt_name = "tableone_temp")

####################################################################################

#generate IPW -first impute the median for missing values
#get data ready 

#missing data
studypop_median <- 1
studypop_completecase <- 0
studypop_cat_missing <- 0

#confounders
confounder_full <- 1

#outcome
outcome_composite <- 0
outcome_heartfailure <- 0
outcome_death <- 0
outcome_mi <- 0
outcome_cancer <- 0

#causal estimand
itt <- 0
per_protocol <- 0
per_protocol_model1 <- 0
per_protocol_model2 <- 0

#choose confounders
source(here("scripts/analysis", "01_an_confounders.R"))

func_weighted_tab1 <- function(input_data) {
  
input_data_prep <- func_analyticdecision (input_data)

#get ipw
ipw <- func_ipweights(input_data = input_data_prep) %>% 
  select(id, ipw_trunc)

#create smd after ipw 
cr_data_weighted <- input_data_prep %>% 
  left_join(ipw, by = "id")  %>% 
  svydesign(ids = ~1, data = ., weight = ~ipw_trunc) 

#assign remaining column names to names (and remove variables that should not be standardised)
vars <- names(input_data_prep)
vars <- vars[!grepl("_tu$|_time$|id|^treatment|^notreatment|index_date|intervention", vars)]

#identify categorical variables
vars_cat <- names(Filter(is.factor, input_data_prep))  

tableone_weighted <- svyCreateTableOne(vars = vars,
                              strata = c("intervention"),
                              factorVars = vars_cat,
                              includeNA = FALSE,
                              data = cr_data_weighted )

cr_tableone_main_temp <- print(tableone_weighted ,quote = FALSE,
                            showAllLevels = TRUE, smd = TRUE, varLabel = FALSE, nonnormal = TRUE,
                            noSpaces = TRUE, printToggle = FALSE)

write.table(cr_tableone_main_temp, here("outputs","cr_tableone_main_temp.txt"), sep="\t", quote = FALSE, row.names=TRUE, col.names=NA)

}

func_weighted_tab1(input_data = cr_data_main)


# formatting

# Make tableone main have  with additional ipweighted smd, and name variables
ipw <-  select(read.delim(here("outputs","cr_tableone_main_temp.txt")), X, SMD) %>% 
  filter(SMD != "") %>% 
  rename("IPW SMD" = SMD)

base <-  (read.delim(here("outputs","tableone_temp.txt"))) %>% 
  left_join(ipw, by ="X") %>% 
  rename("ACE I/ ARB" = "ACE.I.ARB") %>% 
  rename("No ACE I/ ARB" = "No.ACE.I.ARB") %>% 
  relocate(Missing, .before = "SMD") %>% 
  mutate(X = str_remove(X, " \\(%\\)")) %>% 
  mutate(X = str_remove(X,  " \\(median \\[IQR\\]\\)")) %>% 
  mutate(X = case_when(
    X == "n" ~ "N",
    X == "d_age_hia" ~ "Age",
    X == "d_gender" ~ "Female",
    X == "birthcountry" ~ "Country of birth",
    X == "d_civil" ~ "Civil status",   
    X == "occupation_status" ~ "Occupational status",
    X == "dispink04" ~ "Yearly disposable income (1000)",
    X == "sun2000niva_n" ~ "Educational level", 
    X == "smoking_status" ~ "Smoking status",
    X == "previous_mi" ~ "Previous myocardial infarction",
    X == "previous_stroke" ~ "Previous stroke",
    X == "previous_pci" ~ "Previous percutaneous coronary intervention",
    X == "prior_cardiac_surgery" ~ "Cardiac surgery",
    X == "infarcttype" ~ "NSTEMI",
    X == "cpr_before_hospital" ~ "Cardiopulmonary resuscitation", 
    X == "thromb_before_hospital" ~ "Thrombolysis", 
    X == "cardiac_shock" ~ "Cardiogenic shock",
    X == "ecg_rhythm" ~ "ECG ryhthm",
    X == "ecg_qrs_annotation" ~ "ECG QRS annotation",
    X == "ecg_stt_changes" ~ "ECG ST- & T- wave changes",
    X == "d_pci" ~ "Percutaneous coronary intervention",
    X == "fynd" ~ "Angiography finding",
    X == "stenosklass" ~ "Stenosis class",
    X == "segment" ~ "Proportion stenosis",
    X == "d_beta_blockers_treat" ~ "Intravenous beta blockers",
    X == "d_diuretics_treat" ~ "Intravenous diuretics",
    X == "d_inotropes" ~ "Intravenous intropic drugs",
    X == "d_nitrates_treat" ~ "Intravenous nitrates",
    X == "ccb_baseline" ~ "Calcium channel blockers",
    X == "bb_baseline" ~ "Beta Blockers",
    X == "diuretics_baseline" ~ "Diuretics",
    X == "nitrates_baseline" ~ "Nitrates",
    X == "diabetes_treatment_baseline" ~ "Diabetes treatment",
    X == "heart_rate" ~ "Heart rate",
    X == "systolic_blood_pressure" ~ "Systolic blood pressure (mm/Hg)",
    X == "diastolic_blood_pressure" ~ "Diastolic blood pressure (mm/Hg)",
    X == "d_hdl_cholesterol" ~ "HDL cholesterol (mmol/L)",
    X == "d_ldl_cholesterol" ~ "LDL cholesterol (mmol/L)",
    X == "egfr" ~ "Estimated Glomerular Filtration Rate",   
    X == "d_bmi" ~ "Body mass index (kg/m^2)",
    X == "eligible_year" ~ "Year of index",
    X == "centre_name" ~ "Hospital",
    TRUE ~ X
  ))
  
# Replace all NAs with ""
base[is.na(base)] <- ""

# Insert header rows
char <- c("Characteristics and prior diagnoses","","","","","","")
pres <- c("Presentation","","","","","","" )
during <- c("During hospitalization", "","","","","","" )
med <- c("Other medications at baseline", "","","","","","")
measurements <- c("Measurements", "","","","","","","" )

base <- base %>% 
  insertRows(2, char) %>% 
  insertRows(37, pres) %>% 
  insertRows(59, during) %>%
  insertRows(82, med) %>% 
  insertRows(88, measurements)  


write.table(base, 
            "outputs/cr_tableone_main.txt", 
            sep="\t", 
            quote=FALSE, 
            row.names=FALSE)

file.remove((here("outputs","tableone_temp.txt")))
file.remove((here("outputs","cr_tableone_main_temp.txt")))


rm(ipw, base, char, med, pres, during, measurements,  func_cr_tableone, func_weighted_tab1)

