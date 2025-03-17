func_model_standardise <- function(input_data) {
  
  #get data prepped  
  input_data_prep <- func_analyticdecision (input_data = {{input_data}})
  
  #calculate censoring date
  cr_data_censor <- func_censor (input_data = input_data_prep) 
 
  if (per_protocol) {
  
  #generate person time
  persontime_data <- func_persontime(input_data = cr_data_censor) 
  
  #calculate time updated ipw
  ip_weights_adherence <- func_ipw_adherence(input_data = persontime_data)
  
  
  #bring all together - long data and ipw weights for adherence and censoring
  model_data <- inner_join(persontime_data, 
                           select(ip_weights_adherence, id, time, adh_ipw_trunc, adh_ipw), 
                           by = c("id", "time")) 
  }
  
  if (itt) {
    
  #generate person time
    model_data <- func_persontime(input_data = cr_data_censor) 
    
  }

# Number of events -------------------------------------------------------------
    
    events <- model_data %>% 
      group_by(time, intervention) %>% 
      summarise(events = sum(outcome, na.rm = T)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = intervention, values_from = events) %>% 
      rename(events_0 = "0",
             events_1 = "1") %>% 
      mutate(events_0 = cumsum(events_0),
             events_1 = cumsum(events_1))
    
    # Model and estimation of HR, risks, RD, and RR -------------------------------------
    
    model_hr <- as.formula(paste("outcome==1 ~ intervention + 
                       rcs(time, c(6,12,24,48)) +", paste(confounders_baseline, collapse = " + ")))
    
    model_risk <- as.formula(paste("outcome==1 ~ intervention + 
                        intervention*rcs(time, c(6,12,24,48)) +", 
                                   paste(confounders_baseline, collapse = " + ")))
    
    if (itt) {                  
      
      standardize_model_hr <- glm(model_hr,
                                  family=quasibinomial(), 
                                  data=model_data)
      
      standardize_model_risk <- glm(model_risk,
                                    family=quasibinomial(), 
                                    data=model_data)
    }
    
    if (per_protocol) { # Additionally adjust for adherence
      
      standardize_model_hr <- glm(model_hr,
                                  family=quasibinomial(), 
                                  weight=adh_ipw_trunc,
                                  data=model_data)
      
      standardize_model_risk <- glm(model_risk,
                                    family=quasibinomial(), 
                                    weight=adh_ipw_trunc,
                                    data=model_data)
    }
    
    
    #Create datasets with all time points for each individual under each treatment level
    intervention_0 <- expandRows(model_data[model_data$time==0,],
                                 count = max(model_data$time+1),
                                 count.is.col = F)
    intervention_0$intervention <- 0 
    intervention_0$time <- rep(seq(0, max(model_data$time)), nrow(model_data[model_data$time==0,]))
    intervention_0$time_2 <- intervention_0$time^2
    intervention_0$time_3 <- intervention_0$time^3
    intervention_0$intervention <- 0 
    
    intervention_1 <- intervention_0
    intervention_1$intervention <- 1
    
    # assignment of 1-prob of outcome to each person time
    intervention_0$p_0 <- 1 - predict(standardize_model_risk, intervention_0, type="response")
    intervention_1$p_1 <- 1 - predict(standardize_model_risk, intervention_1, type="response")
    
    # survival
    intervention_0 <- intervention_0 %>% 
      group_by(id) %>% 
      mutate(s_0 = cumprod(p_0)) %>% 
      ungroup()
    intervention_1 <- intervention_1 %>% 
      group_by(id) %>% 
      mutate(s_1 = cumprod(p_1)) %>% 
      ungroup()
    
    # Standardize to distribution of baseline covariates
    estimates_0 <- aggregate(intervention_0, 
                             by = list(intervention_0$time), 
                             FUN = mean)[c("intervention", "time", "s_0")]
    estimates_1 <- aggregate(intervention_1, 
                             by = list(intervention_0$time), 
                             FUN = mean)[c("intervention", "time", "s_1")]
    
    # Bring together all estimates
    estimates <- merge(select(estimates_0, time, s_0), 
                       select(estimates_1, time, s_1),
                       by=c("time")) %>% 
      merge(events, by = "time") %>% 
      mutate(cuminc_0 = 1-s_0,
             cuminc_1 = 1-s_1,
             rd = cuminc_1-cuminc_0,
             logrr = log(cuminc_1/cuminc_0)) %>% 
      arrange(time) %>% 
      mutate(loghr = ifelse(time == 0, summary(standardize_model_hr)$coefficients[2,1], NA)) %>% 
      select(time, events_0, events_1, cuminc_0, cuminc_1, rd, logrr, loghr) 
    
  return(estimates)
}

