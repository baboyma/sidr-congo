# PURPOSE: Munge and Analysis of Budget data-now in GIF form!
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-09-17
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(Wavelength)
    library(gophr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(here)
    library(gt)
    library(googlesheets4)
    library(glue)
    library(googledrive)
    library(gganimate)
library(gifski)
    
  
  # Set paths  
     
    
  # Functions  
budget_source <- "FY21Q3i FSD"

# LOAD DATA ============================================================================  

df_fsd <- si_path() %>% 
  return_latest("COP17") %>% 
  read_msd() 
df<-df_fsd
#ou<-"Mozambique"

# MUNGE ============================================================================
get_usaid_graph<-function(df, ou="operatingunit"){
df <- df %>% 
  mutate(funding_agency = "PEPFAR")%>%
    dplyr::mutate(funding_agency = dplyr::case_when(fundingagency    == "USAID"    ~"USAID",
                                                  
                                                  TRUE ~funding_agency))%>%
  dplyr::filter(operatingunit=="Democratic Republic of the Congo")%>%
  
 
    #dplyr::filter(operatingunit %in% ou)%>%
  
  
    remove_mo()%>%
  
    remove_sch("SGAC")
  
  df_fsd1 <- df %>% 
    group_by(fiscal_year,funding_agency) %>% 
    summarise(across(cop_budget_total, sum, na.rm = TRUE)) %>% 
    ungroup()  
   df_fsd1<-df_fsd1%>%
    group_by(fiscal_year) %>% 
    mutate(share = case_when(funding_agency == "USAID" ~ (cop_budget_total/(lag(cop_budget_total)+(cop_budget_total)))),
           total = case_when(funding_agency == "PEPFAR" ~ (cop_budget_total)+lag(cop_budget_total))) %>% 
    ungroup()
   
  df_fsd_usaid<-df_fsd1%>%
    dplyr::filter(funding_agency=="USAID")%>%
    dplyr::select(-share)
  
  df_pepfar_total<-df%>%
    group_by(fiscal_year) %>% 
    summarise(across(cop_budget_total, sum, na.rm = TRUE)) %>% 
    ungroup()%>%
    mutate("funding_agency"="PEPFAR")
    
  
  df_Viz<-bind_rows(df_pepfar_total,df_fsd_usaid)%>%
    group_by(fiscal_year)%>%
    mutate(share = case_when(funding_agency == "USAID" ~ cop_budget_total/lag(cop_budget_total)),
           total = case_when(funding_agency == "PEPFAR" ~ cop_budget_total))%>%
    mutate_at(vars(total),~replace_na(.,0))%>%
  ungroup()
  
  df_graph<-df_Viz %>% 
    ggplot(aes(fiscal_year, cop_budget_total, fill = funding_agency)) + 
    geom_col(aes(fill = funding_agency, alpha = funding_agency), position = "identity") +
    geom_text(aes(label = percent(share, 1)), na.rm = FALSE, vjust = -1, 
              family = "Source Sans Pro SemiBold", color = trolley_grey) +
    geom_errorbar(aes(ymin = total, ymax = total), color = trolley_grey_light, 
                  size = .9, na.rm = TRUE) +
    scale_fill_manual(values = c("USAID" = denim, "PEPFAR" = trolley_grey)) +
    scale_alpha_manual(values = c("USAID" = .8, "PEPFAR" = .4)) +
    scale_y_continuous(labels = unit_format(2, unit = "M", scale = 1e-6),
                       #breaks = seq(0, 1.25e6,.25e6), 
                       position = "right", expand = c(.005, .005)) +
    labs(x = NULL, y = NULL, fill = NULL, alpha = NULL,
         title = "USAID'S Budget Share has remained consistent",
         subtitle = "Annual Country Operation Plan (COP) budgets excluding M&O and Commodities",
         caption = glue("Source: {budget_source}
                        ")) +
    si_style_nolines()
  ggsave("Images/DRC_budget_usaid.png")
  
  graph_gif<-df_graph+
    transition_states(fiscal_year,state_length = 3, wrap = TRUE)+
    transition_reveal(fiscal_year)
  
  animate(graph_gif, renderer=gifski_renderer(), width=1000, height=650, res=92, fps=4)
    anim_save=glue("{ou}_ER_graphs_final.gif")
 
return(df_Viz)
}
anim_save("_SI_graphs_final.gif")
 
    
# VIZ ============================================================================

   drc_graph<-df_DRC_Viz %>% 
   ggplot(aes(fiscal_year, cop_budget_total, fill = funding_agency)) + 
     geom_col(aes(fill = funding_agency, alpha = funding_agency), position = "identity") +
     geom_text(aes(label = percent(share, 1)), na.rm = FALSE, vjust = -1, 
               family = "Source Sans Pro SemiBold", color = trolley_grey) +
     geom_errorbar(aes(ymin = total, ymax = total), color = trolley_grey_light, 
                   size = .9, na.rm = TRUE) +
     scale_fill_manual(values = c("USAID" = denim, "PEPFAR" = trolley_grey)) +
     scale_alpha_manual(values = c("USAID" = .8, "PEPFAR" = .4)) +
     scale_y_continuous(labels = unit_format(2, unit = "M", scale = 1e-6),
                        #breaks = seq(0, 1.25e6,.25e6), 
                        position = "right", expand = c(.005, .005)) +
     labs(x = NULL, y = NULL, fill = NULL, alpha = NULL,
          title = "USAID'S BUDGET SHARE HAS REMAINED CONSISTENT",
          subtitle = "Annual Country Operation Plan (COP) budgets excluding M&O and Commodities",
          caption = glue("Source: {budget_source}
                        ")) +
     si_style_ygrid()
  
  
    graph_gif<-drc_graph+
      transition_states(fiscal_year,state_length = 3, wrap = TRUE)+
      transition_reveal(fiscal_year)
     
    
      animate(graph_gif, height = 500, width = 800, fps = 10, duration = 10,
              end_pause = 60, res = 100,renderer = gifski_renderer())+
       
    anim_save("drc_SI_form_final.gif")
  
 
# SPINDOWN ============================================================================

