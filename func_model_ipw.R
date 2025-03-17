

func_model_ipw <- function(input_data) {
  
  
  #get data ready
  input_data_prep <- func_analyticdecision (input_data = {{input_data}})
  
  if (itt) {
    
    #generate ipw at baseline
    ipw <- func_ipweights(input_data = input_data_prep)
    
    #calculate censoring
    cr_data_censor <- func_censor (input_data = input_data_prep )
    
    #make data long
    cr_data_long <- func_persontime(input_data = cr_data_censor)
    
    #make full ipw dataset
    model_data <- cr_data_long %>% 
      left_join(ipw, by = "id")
    
  } #end itt
  
  if (per_protocol) {
    
    #generate ipw at baseline
    ipw_baseline  <- func_ipweights(input_data = input_data_prep)
    
    #calculate treatment adherence length and censoring
    cr_data_censor <- func_censor (input_data = input_data_prep) 
    
    #generate person time
    persontime_data <- func_persontime(input_data = cr_data_censor) 
    
    #calculate time updated ipw
    ip_weights_adherence <- func_ipw_adherence(input_data = persontime_data)
    
    #bring ipw together
    ip_weights_multiplied <- ipw_baseline %>% 
      left_join(ip_weights_adherence, by = "id") %>% 
      mutate(ipw = ipw * adh_ipw,
             ipw_trunc = ifelse(ipw >= quantile(ipw, 0.99), quantile(ipw, 0.99),
                                ifelse(ipw <= quantile(ipw, 0.01), quantile(ipw, 0.01),
                                       ipw))) %>% 
      select(-contains("adh"))
    
    #bring all together - long data and ipw weights for adherence and censoring
    model_data <- inner_join(persontime_data, 
                             select(ip_weights_multiplied, id, time, ipw, ipw_trunc), 
                             by = c("id", "time"))  
  } #end per protocol 
  
  #generate effect estimates  

  # Number of events -------------------------------------------------------------
  events <- model_data  %>% 
    group_by(time, intervention) %>% 
    summarise(events = sum(outcome, na.rm = T)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = intervention, values_from = events) %>% 
    rename(events_0 = "0",
           events_1 = "1") %>% 
    mutate(events_0 = cumsum(events_0),
           events_1 = cumsum(events_1))
  
  # Model and estimation of risks, RD, and RR -------------------------------------
  # unstabilised weights are used
  
  model_hr <- as.formula(paste("outcome==1 ~ intervention + 
                       rcs(time, c(6,12,24,48))"))
  
  model_risk <- as.formula(paste("outcome==1 ~ intervention + 
                        intervention*rcs(time, c(6,12,24,48))"
  ))
  
  if (trunc == 1) {
    ipw_model_hr <- glm(model_hr,
                        family=quasibinomial(), 
                        weight=ipw_trunc,
                        data=model_data)
    
    ipw_model_risk <- glm(model_risk,
                          family=quasibinomial(), 
                          weight=ipw_trunc,
                          data=model_data)
    
  } else if (trunc == 0) {
    
    ipw_model_hr <- glm(model_hr,
                        family=quasibinomial(), 
                        weight=ipw,
                        data=model_data)
    
    ipw_model_risk <- glm(model_risk,
                          family=quasibinomial(), 
                          weight=ipw,
                          data=model_data)
  }
  
  
  #Create datasets with all time points under each treatment level
  intervention_0 <- data.frame(0, cbind(seq(0,max(model_data$time)), seq(0,max(model_data$time))^2, (seq(0,max(model_data$time))^3)))
  intervention_1 <- data.frame(1, cbind(seq(0,max(model_data$time)), seq(0,max(model_data$time))^2, (seq(0,max(model_data$time))^3)))
  
  colnames(intervention_0) <- c("intervention", "time", "time_2", "time_3")
  colnames(intervention_1) <- c("intervention", "time",  "time_2", "time_3")
  
  # assignment of 1-prob of outcome to each person time
  intervention_0$p_0 <- 1 - predict(ipw_model_risk, intervention_0, type="response")
  intervention_1$p_1 <- 1 - predict(ipw_model_risk, intervention_1, type="response")
  
  # survival
  intervention_0$s_0 <- cumprod(intervention_0$p_0)
  intervention_1$s_1 <- cumprod(intervention_1$p_1)
  
  #merge both datasets together & calculate cuminc
  estimates <- merge(intervention_0, intervention_1, by=c("time", "time_2", "time_3")) %>% 
    merge(events, by = "time") %>% 
    mutate(cuminc_0 = 1-s_0,
           cuminc_1 = 1-s_1,
           rd = cuminc_1-cuminc_0,
           logrr = log(cuminc_1/cuminc_0)) %>% 
    arrange(time) %>% 
    mutate(loghr = ifelse(time == 0, summary(ipw_model_hr)$coefficients[2,1], NA)) %>% 
    select(time, events_0, events_1, cuminc_0, cuminc_1, rd, logrr, loghr) 
  
  rm(model_data, events, model_hr, model_risk, ipw_model_hr, ipw_model_risk, 
     intervention_1, intervention_0)
  
  return(estimates)
 
}


