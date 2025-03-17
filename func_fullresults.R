func_fullresults <- function(estimates, bootstraps_estimates, input_data, location_name, txt_name_1, txt_name_2, txt_name_3,
                             txt_name_4, txt_name_5) {

 #main results
  
  full_results <- {bootstraps_estimates} %>% 
    left_join({estimates}, by = "time") %>% 
    mutate(rr = exp(logrr),
           rr_lci = exp(logrr_lci),
           rr_uci = exp(logrr_uci),
           hr = exp(loghr),
           hr_lci = exp(loghr_lci),
           hr_uci = exp(loghr_uci)
    ) %>% 
    select(time, 
           events_0, events_1,
           cuminc_0, cuminc_0_lci, cuminc_0_uci,
           cuminc_1, cuminc_1_lci, cuminc_1_uci,
           rd, rd_lci, rd_uci,
           rr, rr_lci, rr_uci,
           hr, hr_lci, hr_uci)
  
  risks_all <- full_results %>% 
    select(-c(hr, hr_lci, hr_uci)) %>%
    mutate(cuminc_0 = format(round(cuminc_0*100,1), nsmall=1), 
           cuminc_0_lci = format(round(cuminc_0_lci*100,1), nsmall=1),
           cuminc_0_uci = format(round(cuminc_0_uci*100,1), nsmall=1), 
           cuminc_1 = format(round(cuminc_1*100,1), nsmall=1), 
           cuminc_1_lci = format(round(cuminc_1_lci*100,1), nsmall=1),
           cuminc_1_uci = format(round(cuminc_1_uci*100,1), nsmall=1),
           rd = format(round(rd*100,1), nsmall=1), 
           rd_lci = format(round(rd_lci*100,1), nsmall=1),
           rd_uci = format(round(rd_uci*100,1), nsmall=1), 
           rr = format(round(rr,2), nsmall=2), 
           rr_lci = format(round(rr_lci,2), nsmall=2),
           rr_uci = format(round(rr_uci,2), nsmall=2)) %>% 
    mutate(cuminc_0 = paste0(cuminc_0, " (", cuminc_0_lci,", ", cuminc_0_uci, ")"),
           cuminc_1 = paste0(cuminc_1, " (", cuminc_1_lci,", ", cuminc_1_uci, ")"),
           rd = paste0(rd, " (", rd_lci,", ", rd_uci, ")"),
           rr = paste0(rr, " (", rr_lci,", ", rr_uci, ")"))  %>% 
    select(time, events_1, events_0, cuminc_1, cuminc_0, rd, rr)
  
  write.table(risks_all, here( paste0({{location_name}}) , paste0({{txt_name_1}}, ".txt")), sep="\t", quote=FALSE, row.names=FALSE)
  
  
  # Cut with only the risks at time points of interest and make table ------------
  y <- time_results - 1
  risks_table <- lapply(y, function(x) {
    risks_all %>% 
      filter(time == x) %>% 
      mutate(time = time + 1)
  }) %>% 
    bind_rows()
  
  # Get table with event, max risk difference, and hazard ratio ------------------
  
  hr_table_temp <- select(full_results, time, hr, hr_lci, hr_uci) %>% 
    filter(time == 0) %>% 
    mutate(hr = format(round(hr,2), nsmall=2), 
           hr_lci = format(round(hr_lci,2), nsmall=2),
           hr_uci = format(round(hr_uci,2), nsmall=2)) %>%
    mutate(hr = paste0(hr, " (", hr_lci,", ", hr_uci, ")")) %>% 
    select(hr) 
  
  hr_table_temp_eventsrisks <- risks_table %>% 
    filter(time == as.numeric(max(time_results))) %>% 
    select(time, events_1, events_0, cuminc_1, cuminc_0)
  
  hr_table <- bind_cols(hr_table_temp_eventsrisks, hr_table_temp)
  
  write.table(hr_table, here( paste0({{location_name}}) , paste0({{txt_name_2}}, ".txt")), sep="\t", quote=FALSE, row.names=FALSE)

#-----------------------------------------------------------------------------
# Make IPW plot
  
  x_label <- "Months"
  
  plot_0 <-  full_results %>% 
    mutate(Intervention = as.factor(0),
           cuminc = cuminc_0*100,
           lci = cuminc_0_lci*100,
           uci = cuminc_0_uci*100) %>% 
    select(time, Intervention, cuminc, lci, uci)
  
  plot_1 <- full_results %>% 
    mutate(Intervention = as.factor(1),
           cuminc = cuminc_1*100,
           lci = cuminc_1_lci*100,
           uci = cuminc_1_uci*100) %>% 
    select(time, Intervention, cuminc, lci, uci)
  

  plot <- rbind(plot_1, plot_0) %>% 
    ggplot(aes(x = time, y = cuminc, color = Intervention, fill = Intervention)) +
    geom_smooth(stat = "identity") +
    geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2, linetype = 0) +
    scale_y_continuous(expand = c(0,0),limits = c(0,10.0)) +
    scale_x_continuous(expand = c(0,0), limits = c(0,max(time_results)), breaks = time_results) +
    theme_minimal(base_size = 13) +
    scale_color_nejm(labels = c("ACEi/ARB", "No ACEi/ARB")) +
    scale_fill_nejm(labels = c("ACEi/ARB", "No ACEi/ARB")) +
    xlab("Time") + 
    ylab("Risk (%)") +
    ggtitle(paste0({{txt_name_3}})) +
    theme(plot.title = element_text(hjust = 0.5, size = 14),
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12, colour="black"),
          legend.position = c(0.25, 0.9),
          text = element_text(size = 8, colour="black"),
          plot.margin = margin(15,15,15,15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.x = element_line(color = "black", size = 1.0),  
          axis.line.y = element_line(color = "black", size= 1.0))  
  
  ggsave(paste0({{txt_name_4}}), path = paste0({{location_name}}) )
  
  #-----------------------------------------------------------------------
  #km plot
  
  #main data source (required for km plot), and plot label
  input_data <- {input_data}
  txt_name_5 <- {{txt_name_5}}
  
   if (itt) {
     
    #get data ready
    input_data_prep <- func_analyticdecision (input_data = input_data)
    
      #generate ipw at baseline
      ipw <- func_ipweights(input_data = input_data_prep)
      
      #calculate censoring
      cr_data_censor <- func_censor (input_data = input_data_prep )
      
      #make data long
      cr_data_long <- func_persontime(input_data = cr_data_censor)
      
      #make full ipw dataset
      km_model_data <- cr_data_long %>% 
        left_join(ipw, by = "id")
  
  #generate km plot
  km_dat <- km_model_data %>% 
    group_by(id) %>% 
    select(id, outcome, time, intervention, surv, ipw) %>% 
    mutate(outcome_max = max(outcome)) %>% 
    ungroup() %>% 
    filter(time==0) %>% 
    mutate(outcome_max = ifelse(is.na(outcome_max), 0, outcome_max)) 
  
  survdiff(Surv(surv, outcome_max) ~ intervention, data=km_dat)
  
  fit <- survfit(Surv(surv, outcome_max) ~ intervention, 
                 weights = ipw, #unstabilised
                 data=km_dat)
  
  ipw_km_plot  <- 
    ggsurvplot(fit, 
               data = km_dat, 
               fun = function(x) 100*(1-x),
               xlab="Months of follow-up",
               ylab="Risk (%)",
               main="Product-Limit Survival Estimates", 
               risk.table = TRUE,
               xlim = c(0, max(fit$time)),
               ylim = c(0, 10),
               censor = F,
               legend.title = element_blank(),
               legend.labs = c("ACEi/ ARB","No ACEi/ ARB"),
               legend = c(.2,.9),
               font.legend = c(12, "plain"),
               palette = "nejm")
  
  setwd(paste0({{location_name}}))
  pdf( "ipw_km_plot.pdf")
  print( ipw_km_plot, newpage = FALSE)
  dev.off()
  
  file.rename(from = 'ipw_km_plot.pdf', to = txt_name_5)
  
  setwd("W:/C6_Berglund/ahumphreys/angiotensin/angiotensin project")  }
  
  return(plot)
 
   
} # end function




