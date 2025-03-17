#import results table

# convert string to number format
func_split_string <- function(x) {
  parts <- unlist(strsplit(gsub("[()]", "", x), "[ ,]+"))
  return(as.numeric(parts))
}

#------------------------------------------------------------------------------
#ITT

#import results
composite <- (read.delim(here("outputs/composite/itt", "results_ipw_itt_rr_composite.txt"))) %>% 
  filter(time == max(time)) %>% 
  mutate(estimate = "Composite") 

death <-  (read.delim(here("outputs/death/itt", "results_ipw_itt_rr_death.txt"))) %>% 
  filter(time == max(time)) %>% 
  mutate(estimate = "Death") 

mi <- (read.delim(here("outputs/mi/itt", "results_ipw_itt_rr_mi.txt"))) %>% 
  filter(time == max(time)) %>% 
  mutate(estimate = "MI") 

hf <- (read.delim(here("outputs/hf/itt", "results_ipw_itt_rr_hf.txt"))) %>% 
  filter(time == max(time)) %>% 
  mutate(estimate = "Heart failure") 

result_estimate_itt <- rbind (composite, death, mi, hf) 

rm(composite, death, mi, hf)
#------------------------------------------------------------------------------
#per protocol 

#import results table
composite_pp <- (read.delim(here("outputs/composite/pp", "results_ipw_pp_rr_composite.txt"))) %>% 
  filter(time == max(time)) %>% 
  mutate(estimate = "Composite") 

death_pp <-  (read.delim(here("outputs/death/pp", "results_ipw_pp_rr_death.txt"))) %>% 
  filter(time == max(time)) %>% 
  mutate(estimate = "Death") 

mi_pp <- (read.delim(here("outputs/mi/pp", "results_ipw_pp_rr_mi.txt"))) %>% 
  filter(time == max(time)) %>% 
  mutate(estimate = "MI")

hf_pp <- (read.delim(here("outputs/hf/pp", "results_ipw_pp_rr_hf.txt"))) %>% 
  filter(time == max(time)) %>% 
  mutate(estimate = "Heart failure") 

result_estimate_pp <- rbind (composite_pp, death_pp, mi_pp, hf_pp) 

rm(composite_pp, death_pp, mi_pp, hf_pp)
#------------------------------------------------------------------------------

#create forest plot

func_forestplot <- function(result_estimate, txt_name_1) {
  
result_estimate_plot <- {{result_estimate}} %>% 
    select(estimate, rd)  %>% 
    mutate(rd = sub("^\\s+", "", rd)) %>% #remove any character spaces before
    transform(risk_diff = sapply(rd, function(x) func_split_string(x)[1]),
              rd_lower = sapply(rd, function(x) func_split_string(x)[2]),
              rd_upper = sapply(rd, function(x) func_split_string(x)[3])) 
  
forest_plot <- ggplot(data = result_estimate_plot, aes(x = factor(estimate, levels = c("Heart failure", "MI", "Death", "Composite" )), y = risk_diff, ymin=rd_lower, ymax = rd_upper)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2, size = 0.5) + 
  geom_errorbar(aes(ymin=rd_lower, ymax=rd_upper), width=0.25, cex=0.5)+ 
  coord_flip() +
  geom_point(shape = 15, size = 0.5) + 
  ggtitle("") + 
  xlab("") + 
  ylab("Risk difference (%, 95%CI)") + 
  scale_y_continuous(limits = c(-1.8,1.8), breaks = c(-1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5)) + 
  theme(strip.background = element_rect(fill="white"),
        plot.title = element_text(size = 8),
        legend.position ="none", 
        axis.line.x = element_line(colour = "black", linewidth =  0.75), 
        axis.line.y = element_blank(), 
        panel.border= element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x=element_text(size=8, color = "Black"), 
        axis.text.y=element_blank(), 
        axis.title.x = element_text(size=8, colour = "Black"),
        axis.title.y = element_text(size = 8, colour = "Black"),
        aspect.ratio = 2/3) 

#create a table
result_estimate_overall <- {{result_estimate}} %>% 
  select(-c(time, events_1, events_0)) %>%
  tidyr::pivot_longer(c(cuminc_1, cuminc_0, rr, rd ), names_to = "stat") %>%
  mutate(stat = factor(stat, levels = c("cuminc_1", "cuminc_0", "rr", "rd")))


#indented labels
table_base <- ggplot(result_estimate_overall, aes(stat, factor(estimate, levels = c("Heart failure", "MI", "Death", "Composite" )) , label = value)) +
  geom_text(size = 7 * 0.3528) +
  scale_x_discrete(position = "top", labels = c("Risk ACEi/ARB \n (%, 95%CI)", "Risk no ACEi/ARB \n (%, 95%CI)","Risk ratio \n (95%CI)", "Risk difference \n (%, 95%CI)")) +
  labs(y = NULL, x = NULL) +
  theme_classic() +
  annotate("text", x = 0, y = 1, label = expression(bold("HF")), hjust = 0, size = 7 * 0.3528 ) +
  annotate("text", x = 0, y = 2, label = expression(bold("MI")), hjust = 0, size = 7 * 0.3528 ) +
  annotate("text", x = 0, y = 3, label = expression(bold("Death")), hjust = 0, size = 7 * 0.3528 ) +
  annotate("text", x = -0.15, y = 4, label = expression(bold("Composite")), hjust = 0, size = 7 * 0.3528 ) +
  theme(
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 7 , face = "bold"),
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold"),
aspect.ratio = 1/3)

table_base + forest_plot + plot_layout(widths = c(2, 1)) 

ggsave(paste0({{txt_name_1}}), path = "outputs") 

}

func_forestplot(result_estimate = result_estimate_itt, txt_name_1= "forest_plot_itt.pdf")

func_forestplot(result_estimate = result_estimate_pp, txt_name_1= "forest_plot_pp.pdf")

rm(func_forestplot,result_estimate_itt,result_estimate_pp)


  