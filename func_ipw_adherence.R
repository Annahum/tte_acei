# CREATE IP WEIGHTS FOR CENSORING DUE TO ADHERENCE
# BASED ON Ellie and Anthony's codes

#### ESTIMATION OF TIME VARYING IP WEIGHTS

func_ipw_adherence <- function(input_data,  botharms = F) {
  
  # Create adherence (instead of censor) variable and adherence at baseline  
  dat_full <- input_data %>% 
    mutate(adhr = 1 - ppcensor) %>% 
    mutate(adhr_base = ifelse(time==0, adhr, NA)) %>% 
    group_by(id) %>% 
    mutate(adhr_base = max(adhr_base, na.rm = TRUE)) %>% 
    ungroup() 
  
 # specify models
 # for the numerator, as dynamic treatment strategy choosing unstabilised weights 
  model_denominator <- paste("adhr ~ intervention + rcs(time, c(6,12,24,48)) + ", paste(confounders_timeupdated, collapse = " + "), sep = "")
  

#----------------------------------------------------------------------------
  
# Option 1 - Fit models in full data
  if(!botharms) {

    #ipw denominator model
    p_denom <- glm(model_denominator, 
                   family=binomial(), data=dat_full)
    
    
    weights <- dat_full %>% 
      mutate(denom = predict(p_denom, dat_full,  type="response")) %>% 
      select(id, time, adhr, denom) %>% 
      mutate(
        denomcont = adhr*denom + (1-adhr)*(1-denom), 
        denomcont = ifelse(time==0, 1, denomcont)
      ) %>%
      group_by(id) %>%
      arrange(time) %>% 
      mutate(
        k1_w = cumprod(denomcont),
      ) %>% 
      ungroup() %>% 
      mutate(
        #create weights (no stabilised)
        adh_ipw = 1/k1_w, 
        
        #truncate to 1st and 99th percentiles
        adh_ipw_trunc = ifelse(adh_ipw >= quantile(adh_ipw, 0.99), quantile(adh_ipw, 0.99),
                               ifelse(adh_ipw <= quantile(adh_ipw, 0.01), quantile(adh_ipw, 0.01),
                                      adh_ipw))
      ) %>% 
      select(id, time, adh_ipw, adh_ipw_trunc)
    
    rm(dat_full, model_denominator, p_denom)
    gc()
    
    return(weights)
  

#-------------------------------------------------------------------------------    
       
    # Option 2 - Fit models in each arm   because reasons fore ahderence may vary between arm
  }  else if(botharms) {
    
    # Data for each exposure arm
    dat_exp0 <- dat_full %>% 
      filter(intervention==0)
    
    dat_exp1 <- dat_full %>% 
      filter(intervention==1)
    
    #ipw denominator model
    p_denom_0 <- glm(model_denominator, 
                     family=binomial(), data=dat_exp0)
    p_denom_1 <- glm(model_denominator, 
                     family=binomial(), data=dat_exp1)
    
    #predict denominator for each treatment group
    weights_0 <- dat_exp0 %>% 
      mutate(denom = predict(p_denom_0, dat_exp0,  type="response")) %>% 
      select(id, time, adhr, denom) %>% 
      mutate(
        denomcont = adhr*denom + (1-adhr)*(1-denom), 
        denomcont = ifelse(time==0, 1, denomcont)
      ) %>%
      group_by(id) %>%
      arrange(time) %>% 
      mutate(
        k1_w = cumprod(denomcont)
      ) %>% 
      ungroup() %>% 
      mutate(
        #create weights
        adh_ipw = 1 /k1_w, 
        
        #truncate to 1st and 99th percentiles
        adh_ipw_trunc = ifelse(adh_ipw >= quantile(adh_ipw, 0.99), quantile(adh_ipw, 0.99),
                               ifelse(adh_ipw <= quantile(adh_ipw, 0.01), quantile(adh_ipw, 0.01),
                                      adh_ipw))
      ) %>% 
      select(id, time, adh_ipw, adh_ipw_trunc, adh_s_ipw, adh_s_ipw_trunc)
    
    weights_1 <- dat_exp1 %>% 
      mutate(denom = predict(p_denom_1, dat_exp1,  type="response")) %>% 
      select(id, time, adhr, denom) %>% 
      mutate(
        denomcont = adhr*denom + (1-adhr)*(1-denom), 
        denomcont = ifelse(time==0, 1, denomcont)
      ) %>%
      group_by(id) %>%
      arrange(time) %>% 
      mutate(
        k1_w = cumprod(denomcont)) %>% 
      ungroup() %>% 
      mutate(
        #create weights
        adh_ipw = 1 /k1_w, 
        
        #truncate to 1st and 99th percentiles
        adh_ipw_trunc = ifelse(adh_ipw >= quantile(adh_ipw, 0.99), quantile(adh_ipw, 0.99),
                               ifelse(adh_ipw <= quantile(adh_ipw, 0.01), quantile(adh_ipw, 0.01),
                                      adh_ipw))
      ) %>% 
      select(id, time, adh_ipw, adh_ipw_trunc, adh_s_ipw, adh_s_ipw_trunc)
    
    rm(dat_full, dat_exp0, dat_exp1, model_denominator,
       p_denom_0, p_denom_1,)
    gc()
    
    return(rbind(weights_0, weights_1))
  }

  }

