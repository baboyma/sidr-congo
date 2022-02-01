# PURPOSE: Munge and Analysis of Peds data in DRC
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-11-23
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glamr)
    library(gophr)
    library(tidyverse)
    library(tidytext)
    library(patchwork)
    library(gt)
    library(googlesheets4)
    library(glue)
    library(googledrive)
    
    
  

    
  # Functions  
indc2 <- c("TX_CURR", "TX_NET_NEW", "TX_NEW", "HTS_TST", "HTS_TST_POS", "TX_RTT", "TX_ML")

ind_list <- c("TX_CURR", "TX_NET_NEW", "retention",
              "TX_NEW", "linkage", "HTS_TST_POS", "positivity",
              "HTS_TST", "TX_PVLS_D", "coverage", "suppression",
              "TX_ML", "TX_RTT")

pd_list <- c("fy2012q1",
             "fy2012q2",
             "fy2012q3",
             "fy2012q4",
             "fy2012cumulative",
             "fy2012_targets") 
funds<-c("USAID","HHS/CDC")
  

# LOAD DATA ============================================================================  

df_msd<-si_path()%>%
  return_latest("Congo")%>%
  gophr::read_msd()
#step 1: everything other than vl/retention
df_peds<-df_msd%>%
  filter(fiscal_year=="2021")%>%
  dplyr::filter(trendscoarse=="<15")%>%
  filter(fundingagency %in% funds)%>%
  filter(indicator %in% indc2) %>%
  select( fiscal_year,fundingagency, standardizeddisaggregate,  indicator, targets:cumulative) %>%
  group_by(fiscal_year,fundingagency, standardizeddisaggregate,  indicator,) %>% 
  summarise_if(is.numeric,  ~sum(., na.rm = TRUE)) %>%
  ungroup() %>% 
  reshape_msd("long")


#step 2: retention/vl

df_vl <- df_msd %>%
  filter(indicator %in% c("TX_CURR", "TX_PVLS"),
         fiscal_year %in% c(2021),
         fundingagency %in% funds,
        trendscoarse=="<15" ) %>%
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator),
         indicator = ifelse(numeratordenom == "N" , paste0(indicator, "_N"), indicator),
         indicator = case_when(indicator == "TX_CURR_N" ~ "TX_CURR",
                               TRUE ~ indicator)) %>% 
  select(fundingagency, standardizeddisaggregate,  indicator, fiscal_year, starts_with("qtr")) %>%
  group_by(fundingagency, standardizeddisaggregate,   fiscal_year, indicator) %>% 
  summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>%
  ungroup() %>% 
  reshape_msd("long") %>% 
  spread(indicator, value) %>% 
  group_by(fundingagency,standardizeddisaggregate,) %>% 
  mutate(TX_CURR_lag2 = lag(TX_CURR, 2),
         TX_CURR_lag1 = lag(TX_CURR, 1)) %>%
  select(-TX_CURR) %>% 
  ungroup() %>% 
  gather(indicator, value = value, TX_PVLS_D:TX_CURR_lag1, na.rm = TRUE)

##  step 3: append

df_drc <- bind_rows(df_peds, df_vl)

##  Step 4: create summary table, create vl suppression, coverage, retention
## summary table
df_drc<-df_drc %>%
 # filter(standardizeddisaggregate %in% c("Age/Sex/HIVStatus","Age/Sex/Indication/HIVStatus")) %>% 
  group_by(fundingagency,standardizdisaggregate,indicator, period,period_type) %>% 
  summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) 
df_drc<-df_drc%>%
  spread(indicator, value) 
df_drc<-df_drc%>%
  mutate(suppression = TX_PVLS_D/TX_CURR_lag2,
         coverage = TX_PVLS_N/TX_PVLS_D,
         retention = TX_CURR/(TX_CURR_lag1+TX_NEW),
         linkage = round(TX_NEW/HTS_TST_POS,2),
         positivity = round(HTS_TST_POS/HTS_TST, 3)) 
df_drc<-df_drc%>%
  ungroup() %>% 
  select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N)  
 
  df_drc<-df_drc%>%
  tidyr::pivot_longer(cols=HTS_TST: positivity
                      ,names_to ="indicator",
                     values_to = "value") 
  df_drc$periods<-paste(df_drc$period,"-",df_drc$period_type)
  df4<-df_drc%>%
    dplyr::select(fundingagency, indicator:periods)%>%
    tidyr::pivot_wider(names_from="periods",
                       values_from="value")
  

df4<-df4%>%
  mutate("FY21-gap"=`FY21 - targets`-`FY21 - cumulative`)%>%
  mutate("FY21-achievement"=`FY21 - cumulative`/`FY21 - targets`)

write.csv(df4, "Peds data DRC.csv")


##recreate the above with SNUS=====

#step 1: everything other than vl/retention
df_peds<-df_msd%>%
  filter(fiscal_year=="2021")%>%
  dplyr::filter(trendscoarse=="<15")%>%
  filter(fundingagency %in% funds)%>%
  filter(indicator %in% indc2) %>%
  select( fiscal_year,snu1, standardizeddisaggregate,  indicator, targets:cumulative) %>%
  group_by(fiscal_year,snu1, standardizeddisaggregate,  indicator,) %>% 
  summarise_if(is.numeric,  ~sum(., na.rm = TRUE)) %>%
  ungroup() %>% 
  reshape_msd("long")


#step 2: retention/vl

df_vl <- df_msd %>%
  filter(indicator %in% c("TX_CURR", "TX_PVLS"),
         fiscal_year %in% c(2021),
         fundingagency %in% funds,
         trendscoarse=="<15" ) %>%
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator),
         indicator = ifelse(numeratordenom == "N" , paste0(indicator, "_N"), indicator),
         indicator = case_when(indicator == "TX_CURR_N" ~ "TX_CURR",
                               TRUE ~ indicator)) %>% 
  select(snu1, standardizeddisaggregate,  indicator, fiscal_year, starts_with("qtr")) %>%
  group_by(snu1, standardizeddisaggregate,   fiscal_year, indicator) %>% 
  summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>%
  ungroup() %>% 
  reshape_msd("long") %>% 
  spread(indicator, value) %>% 
  group_by(snu1,standardizeddisaggregate,) %>% 
  mutate(TX_CURR_lag2 = lag(TX_CURR, 2),
         TX_CURR_lag1 = lag(TX_CURR, 1)) %>%
  select(-TX_CURR) %>% 
  ungroup() %>% 
  gather(indicator, value = value, TX_PVLS_D:TX_CURR_lag1, na.rm = TRUE)

##  step 3: append

df_drc <- bind_rows(df_peds, df_vl)

##  Step 4: create summary table, create vl suppression, coverage, retention
## summary table
df_drc<-df_drc %>%
  # filter(standardizeddisaggregate %in% c("Age/Sex/HIVStatus","Age/Sex/Indication/HIVStatus")) %>% 
  group_by(snu1,indicator, period,period_type) %>% 
  summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) 
df_drc<-df_drc%>%
  spread(indicator, value) 
df_drc<-df_drc%>%
  mutate(suppression = TX_PVLS_D/TX_CURR_lag2,
         coverage = TX_PVLS_N/TX_PVLS_D,
         retention = TX_CURR/(TX_CURR_lag1+TX_NEW),
         linkage = round(TX_NEW/HTS_TST_POS,2),
         positivity = round(HTS_TST_POS/HTS_TST, 3)) 
df_drc<-df_drc%>%
  ungroup() %>% 
  select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N)  

df_drc<-df_drc%>%
  tidyr::pivot_longer(cols=HTS_TST: positivity
                      ,names_to ="indicator",
                      values_to = "value") 
df_drc$periods<-paste(df_drc$period,"-",df_drc$period_type)
df5<-df_drc%>%
  dplyr::select(snu1, indicator:periods)%>%
  tidyr::pivot_wider(names_from="periods",
                     values_from="value")


df5<-df5%>%
  mutate("FY21-gap"=`FY21 - targets`-`FY21 - cumulative`)%>%
  mutate("FY21-achievement"=`FY21 - cumulative`/`FY21 - targets`)

write.csv(df5, "Peds data DRC_by_SNU.csv")
##  PROJECT: jacquie table
##  AUTHOR:  jdavis | USAID
##  PURPOSE: code to create table of results a la jacquie
##  LICENCE: MIT
##  DATE:    2020-6-16
##  UPDATE:  2020-11-09

#-------------------------------------------------------
library(tidyverse)
library(ICPIutilities)

#------------------------------------------------------

data_out <- "C:/Users/Josh/Documents/data/fy20_q2_v1/zim"
data <- "C:/Users/Josh/Documents/data/fy20_q2_v1"

#read in data------------------------------------------------------

# df <- MSD
df <- list.files(
  path = "../../MERDATA",
  #pattern = "MER_S.*_OU_IM_.*_20200814_.*.zip",
  pattern = "MER_S.*_PSNU_IM_.*_20200814_.*.zip",
  full.names = TRUE
) %>% 
  first() %>% 
  read_msd()

#globals-----------------------------------------------------------

prinf <- function(df) {
  print(df, n = Inf)
}

indc2 <- c("TX_CURR", "TX_NET_NEW", "TX_NEW", "HTS_TST", "HTS_TST_POS", "TX_RTT", "TX_ML")

ind_list <- c("TX_CURR", "TX_NET_NEW", "retention",
              "TX_NEW", "linkage", "HTS_TST_POS", "positivity",
              "HTS_TST", "TX_PVLS_D", "coverage", "suppression",
              "TX_ML", "TX_RTT")

pd_list <- c("fy2019q1",
             "fy2019q2",
             "fy2019q3",
             "fy2019q4",
             "fy2019cumulative",
             "fy2019_targets",
             "fy2020q1",
             "fy2020q2",
             "fy2020cumulative",
             "fy2020_targets")

#step 1: everything other than vl/retention

df1 <- df %>% 
  filter(
    indicator %in% indc2,
    fiscal_year %in% c(2021),
    fundingagency %in% funds) %>% #fundingagency=="USAID"
  select(fundingagency, snu1, standardizeddisaggregate, otherdisaggregate, sex, trendsfine, indicator, fiscal_year, targets:cumulative) %>%
  group_by(fundingagency, snu1, standardizeddisaggregate, otherdisaggregate, sex, trendsfine, fiscal_year, indicator) %>% 
  summarise_if(is.numeric,  ~sum(., na.rm = TRUE)) %>%
  ungroup() %>% 
  reshape_msd("long")

#step 2: retention/vl

df_vl <- df %>%
  filter(indicator %in% c("TX_CURR", "TX_PVLS"),
         fiscal_year %in% c(2021),
         fundingagency=="USAID") %>% #fundingagency=="USAID"
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator),
         indicator = ifelse(numeratordenom == "N" , paste0(indicator, "_N"), indicator),
         indicator = case_when(indicator == "TX_CURR_N" ~ "TX_CURR",
                               TRUE ~ indicator)) %>% 
  select(fundingagency, snu1, standardizeddisaggregate, sex, trendsfine, indicator, fiscal_year, starts_with("qtr")) %>%
  group_by(fundingagency, snu1, standardizeddisaggregate, sex, trendsfine, fiscal_year, indicator) %>% 
  summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>%
  ungroup() %>% 
  reshape_msd("long") %>% 
  spread(indicator, value) %>% 
  group_by(fundingagency, snu1, standardizeddisaggregate, sex, trendsfine) %>% 
  mutate(TX_CURR_lag2 = lag(TX_CURR, 2),
         TX_CURR_lag1 = lag(TX_CURR, 1)) %>%
  select(-TX_CURR) %>% 
  ungroup() %>% 
  gather(indicator, value = value, TX_PVLS_D:TX_CURR_lag1, na.rm = TRUE)

##  step 3: append

df_drc <- bind_rows(df1, df_vl)


##  Step 4: create summary table, create vl suppression, coverage, retention
## summary table
df_drc %>%
  filter(standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
  group_by(fundingagency, indicator, period) %>% 
  summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
  spread(indicator, value) %>%
  mutate(suppression = TX_PVLS_D/TX_CURR_lag2,
         coverage = TX_PVLS_N/TX_PVLS_D,
         retention = TX_CURR/(TX_CURR_lag1+TX_NEW),
         linkage = round(TX_NEW/HTS_TST_POS,2)*100,
         positivity = round(HTS_TST_POS/HTS_TST, 3)*100) %>%
  ungroup %>% 
  select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N) %>% 
  gather(indicator, value, HTS_TST:positivity) %>%
  mutate(indicator = factor(indicator, ind_list),
         period = factor(period, pd_list))%>%  #,
        # fundingagency = str_remove(fundingagency, "HHS/"),
         #fundingagency = factor(fundingagency, c("USAID", "CDC"))) %>% 
  spread(period, value)

# write_csv(file.path(data_out, "zim_overview_tbl_q2.csv"))


## table for peds
df_drcp<-df_drc
df_drcp$periods<-paste(df_drcp$period,"-",df_drcp$period_type)
df_drcp<-df_drcp%>%
  filter(trendsfine %in% c("<01", "01-09", "10-14"))%>%
  group_by(fundingagency,snu1, indicator, periods) %>% 
  summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
  spread(indicator, value) %>%
  mutate(suppression = TX_PVLS_D/TX_CURR_lag2,
         coverage = TX_PVLS_N/TX_PVLS_D,
         retention = TX_CURR/(TX_CURR_lag1 + TX_NEW),
         linkage = round(TX_NEW / HTS_TST_POS, 2) ,
         positivity = round(HTS_TST_POS / HTS_TST, 3) ) %>%
  ungroup %>% 
  select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N) %>% 
  gather(indicator, value, HTS_TST:positivity) %>%
  spread(periods, value)
df_drcp<-df_drcp%>%
  mutate("FY21-gap"=`FY21 - targets`-`FY21 - cumulative`)%>%
  mutate("FY21-achievement"=`FY21 - cumulative`/`FY21 - targets`)

df_drcp<-df_drcp%>%
write_csv( "drc_overview_tbl_peds_q4_usaid.csv")







