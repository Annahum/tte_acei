# Bootstrapping 

#bootstrapping function (also for new id)
random.resample <- function(df_1) {
  resampled_df <- df_1[sample(1:nrow(df_1), nrow(df_1), replace = TRUE), ]
  resampled_df$id <- 1:nrow(resampled_df) #force everyone to have a new id
  return(resampled_df)
}

#bootstrap ipw model

func_bootstrap_ipw <- function(n, input_data) {
  set.seed(1)   
  
  replicate({{n}}, random.resample( df_1 = {{input_data}} ), FALSE) %>% 
    map(~ func_model_ipw(input_data = .)) %>%
    bind_rows() %>%
    group_by(time) %>% 
    summarise(cuminc_0_lci = quantile(cuminc_0, probs = 0.025, na.rm = T),
              cuminc_0_uci = quantile(cuminc_0, probs = 0.975, na.rm = T),
              cuminc_1_lci = quantile(cuminc_1, probs = 0.025, na.rm = T),
              cuminc_1_uci = quantile(cuminc_1, probs = 0.975, na.rm = T),
              rd_lci = quantile(rd, probs = 0.025, na.rm = T),
              rd_uci = quantile(rd, probs = 0.975, na.rm = T),
              logrr_lci = quantile(logrr, probs = 0.025, na.rm = T),
              logrr_uci = quantile(logrr, probs = 0.975, na.rm = T),
              loghr_lci = quantile(loghr, probs = 0.025, na.rm = T),
              loghr_uci = quantile(loghr, probs = 0.975, na.rm = T))
}

#bootstrap standardisation
func_bootstrap_standardise <- function(n, input_data) {
  set.seed(1)   
  
  replicate({{n}}, random.resample( {{input_data}} ), FALSE) %>% 
    map(~ func_model_standardise (input_data = .))  %>%
    bind_rows() %>%
    group_by(time) %>% 
    summarise(cuminc_0_lci = quantile(cuminc_0, probs = 0.025, na.rm = T),
              cuminc_0_uci = quantile(cuminc_0, probs = 0.975, na.rm = T),
              cuminc_1_lci = quantile(cuminc_1, probs = 0.025, na.rm = T),
              cuminc_1_uci = quantile(cuminc_1, probs = 0.975, na.rm = T),
              rd_lci = quantile(rd, probs = 0.025, na.rm = T),
              rd_uci = quantile(rd, probs = 0.975, na.rm = T),
              logrr_lci = quantile(logrr, probs = 0.025, na.rm = T),
              logrr_uci = quantile(logrr, probs = 0.975, na.rm = T),
              loghr_lci = quantile(loghr, probs = 0.025, na.rm = T),
              loghr_uci = quantile(loghr, probs = 0.975, na.rm = T))
}
