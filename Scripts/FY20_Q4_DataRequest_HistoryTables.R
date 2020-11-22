##  PROJECT: Performance Summary Tables
##  AUTHOR:  jdavis | USAID
##  PURPOSE: code to create table of results a la jacquie
##  LICENCE: MIT
##  DATE:    2020-6-16
##  UPDATE:  2020-11-20

# PACKAGES -------------------------------------------------

library(tidyverse)
library(ICPIutilities)

# GLOBAL --------------------------------------------------

# OU/Country

country <- "Democratic Republic of the Congo"

agencies <- c("USAID", "HHS/CDC")

curr_pd <- 4

curr_fy <- 2020 #Wavelength::curr_fy

fiscal_years <- c(curr_fy - 1, curr_fy)
#fiscal_years <- c(2019, 2020)

# Indicators ----------------------------------------------------

ind_list1 <- c("TX_CURR",
              "TX_NET_NEW",
              "TX_NEW",
              "HTS_TST",
              "HTS_TST_POS",
              "TX_RTT",
              "TX_ML")

ind_list2 <- c("TX_CURR", "TX_NET_NEW", "retention",
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
             "fy2020q3",
             "fy2020q4",
             "fy2020cumulative",
             "fy2020_targets")

# DATA ------------------------------------------------------

# df <- MSD
df <- list.files(
    path = dir_merdata,
    pattern = "MER_S.*_PSNU_IM_.*_20201113_v1_1.zip",
    full.names = TRUE
  ) %>%
  sort() %>%
  last() %>%
  read_msd()

df %>% glimpse()

#step 1: everything other than vl/retention

df1 <- df %>%
  filter(operatingunit == country,
         indicator %in% ind_list1,
         fiscal_year %in% fiscal_years,
         fundingagency != "Dedup") %>%
  dplyr::select(fundingagency, snu1, psnu, psnuuid,
                standardizeddisaggregate, otherdisaggregate, sex,
                trendsfine, indicator, fiscal_year, targets:cumulative) %>%
  group_by(fundingagency, snu1, psnu, psnuuid, standardizeddisaggregate,
           otherdisaggregate, sex, trendsfine, fiscal_year, indicator) %>%
  summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
  ungroup() %>%
  reshape_msd("long")

#step 2: retention/vl

df_vl <- df %>%
  filter(indicator %in% c("TX_CURR", "TX_PVLS"),
         fiscal_year %in% fiscal_years,
         fundingagency != "Dedup",
         operatingunit == country) %>%
  mutate(indicator = ifelse(numeratordenom == "D",
                            paste0(indicator, "_D"),
                            indicator),
         indicator = ifelse(numeratordenom == "N",
                            paste0(indicator, "_N"),
                            indicator),
         indicator = case_when(
           indicator == "TX_CURR_N" ~ "TX_CURR",
           TRUE ~ indicator)) %>%
  dplyr::select(fundingagency, snu1, psnu, psnuuid, standardizeddisaggregate,
                sex, trendsfine, indicator, fiscal_year, starts_with("qtr")) %>%
  group_by(fundingagency, snu1, psnu, psnuuid, standardizeddisaggregate, sex,
           trendsfine, fiscal_year, indicator) %>%
  summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>%
  ungroup() %>%
  reshape_msd("long") %>%
  spread(indicator, val) %>%
  group_by(fundingagency, snu1, psnu, psnuuid,
           standardizeddisaggregate, sex, trendsfine) %>%
  mutate(TX_CURR_lag2 = lag(TX_CURR, 2),
         TX_CURR_lag1 = lag(TX_CURR, 1)) %>%
  dplyr::select(-TX_CURR) %>%
  ungroup() %>%
  gather(indicator, value = val, TX_PVLS_D:TX_CURR_lag1, na.rm = TRUE)


##  step 3: append

df_cntry <- bind_rows(df1, df_vl)

##  Step 4: create summary table, create vl suppression, coverage, retention
## summary table

df_cntry_all <- df_cntry %>%
  filter(
    standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
    fundingagency %in% agencies
  ) %>%
  group_by(fundingagency, indicator, period) %>%
  summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
  spread(indicator, val) %>%
  mutate(suppression = round(TX_PVLS_D / TX_CURR_lag2 * 100, 2),
         coverage = round(TX_PVLS_N / TX_PVLS_D * 100, 2),
         retention = round(TX_CURR / (TX_CURR_lag1 + TX_NEW) * 100, 2),
         linkage = round(TX_NEW / HTS_TST_POS * 100, 2),
         positivity = round(HTS_TST_POS / HTS_TST * 100, 2)) %>%
  ungroup %>%
  dplyr::select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N) %>%
  gather(indicator, val, HTS_TST:positivity) %>%
  mutate(indicator = factor(indicator, ind_list2),
         period = factor(period, pd_list),
         fundingagency = str_remove(fundingagency, "HHS/"),
         fundingagency = factor(fundingagency, c("USAID", "CDC"))) %>%
  spread(period, val)

# Export Country summary tbl
write_csv(x = df_cntry_all,
          file = file.path(dir_dataout,
                           paste0(country,
                                  " - Overview_tbl_q",
                                  curr_pd,
                                  "_", format(Sys.Date(), "%Y%m%d"),
                                  ".csv")))

# SNU1 Summary
df_snu1_all <- df_cntry %>%
  filter(
    standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
    fundingagency %in% agencies
  ) %>%
  group_by(fundingagency, snu1, indicator, period) %>%
  summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
  spread(indicator, val) %>%
  mutate(suppression = round(TX_PVLS_D / TX_CURR_lag2 * 100, 2),
         coverage = round(TX_PVLS_N / TX_PVLS_D * 100, 2),
         retention = round(TX_CURR / (TX_CURR_lag1 + TX_NEW) * 100, 2),
         linkage = round(TX_NEW / HTS_TST_POS * 100, 2),
         positivity = round(HTS_TST_POS / HTS_TST * 100, 2)) %>%
  ungroup %>%
  dplyr::select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N) %>%
  gather(indicator, val, HTS_TST:positivity) %>%
  mutate(indicator = factor(indicator, ind_list2),
         period = factor(period, pd_list),
         fundingagency = str_remove(fundingagency, "HHS/"),
         fundingagency = factor(fundingagency, c("USAID", "CDC"))) %>%
  spread(period, val)

# Export snu1 summary tbl
write_csv(x = df_snu1_all,
          file = file.path(dir_dataout,
                           paste0(country,
                                  " - Overview_snu1_tbl_q",
                                  curr_pd,
                                  "_", format(Sys.Date(), "%Y%m%d"),
                                  ".csv")))

# PSNU Summary
df_psnu_all <- df_cntry %>%
  filter(
    standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
    fundingagency %in% agencies
  ) %>%
  group_by(fundingagency, snu1, psnu, psnuuid, indicator, period) %>%
  summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
  spread(indicator, val) %>%
  mutate(suppression = round(TX_PVLS_D / TX_CURR_lag2 * 100, 2),
         coverage = round(TX_PVLS_N / TX_PVLS_D * 100, 2),
         retention = round(TX_CURR / (TX_CURR_lag1 + TX_NEW) * 100, 2),
         linkage = round(TX_NEW / HTS_TST_POS * 100, 2),
         positivity = round(HTS_TST_POS / HTS_TST * 100, 2)) %>%
  ungroup %>%
  dplyr::select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N) %>%
  gather(indicator, val, HTS_TST:positivity) %>%
  mutate(indicator = factor(indicator, ind_list2),
         period = factor(period, pd_list),
         fundingagency = str_remove(fundingagency, "HHS/"),
         fundingagency = factor(fundingagency, c("USAID", "CDC"))) %>%
  spread(period, val)

# Export psnu summary tbl
write_csv(x = df_psnu_all,
          file = file.path(dir_dataout,
                           paste0(country,
                                  " - Overview_psnu_tbl_q",
                                  curr_pd,
                                  "_", format(Sys.Date(), "%Y%m%d"),
                                  ".csv")))



## table for peds
df_cntry_peds <- df_cntry %>%
  filter(trendsfine %in% c("<01", "01-09", "10-14"),
         fundingagency %in% agencies) %>%
  group_by(fundingagency, indicator, period) %>%
  summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
  spread(indicator, val) %>%
  mutate(suppression = round(TX_PVLS_D / TX_CURR_lag2 * 100, 2),
         coverage = round(TX_PVLS_N / TX_PVLS_D * 100, 2),
         retention = round(TX_CURR / (TX_CURR_lag1 + TX_NEW) * 100, 2),
         linkage = round(TX_NEW / HTS_TST_POS * 100, 2),
         positivity = round(HTS_TST_POS / HTS_TST * 100, 2)) %>%
  ungroup %>%
  dplyr::select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N) %>%
  gather(indicator, val, HTS_TST:positivity) %>%
  mutate(indicator = factor(indicator, ind_list2),
         period = factor(period, pd_list),
         fundingagency = str_remove(fundingagency, "HHS/"),
         fundingagency = factor(fundingagency, c("USAID", "CDC"))) %>%
  spread(period, val)

# Export
write_csv(x = df_cntry_peds,
          file = file.path(dir_dataout,
                           paste0(country,
                                  " - Overview_peds_tbl_q",
                                  curr_pd,
                                  "_", format(Sys.Date(), "%Y%m%d"),
                                  ".csv")))


## table for peds x snu1
df_cntry_peds_by_snu1 <- df_cntry %>%
  filter(trendsfine %in% c("<01", "01-09", "10-14"),
         fundingagency %in% agencies) %>%
  group_by(fundingagency, snu1, indicator, period) %>%
  summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
  spread(indicator, val) %>%
  mutate(suppression = round(TX_PVLS_D / TX_CURR_lag2 * 100, 2),
         coverage = round(TX_PVLS_N / TX_PVLS_D * 100, 2),
         retention = round(TX_CURR / (TX_CURR_lag1 + TX_NEW) * 100, 2),
         linkage = round(TX_NEW / HTS_TST_POS * 100, 2),
         positivity = round(HTS_TST_POS / HTS_TST * 100, 2)) %>%
  ungroup %>%
  dplyr::select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N) %>%
  gather(indicator, val, HTS_TST:positivity) %>%
  mutate(indicator = factor(indicator, ind_list2),
         period = factor(period, pd_list),
         fundingagency = str_remove(fundingagency, "HHS/"),
         fundingagency = factor(fundingagency, c("USAID", "CDC"))) %>%
  spread(period, val)

# Export
write_csv(x = df_cntry_peds_by_snu1,
          file = file.path(dir_dataout,
                           paste0(country,
                                  " - Overview_peds_x_snu1_tbl_q",
                                  curr_pd,
                                  "_", format(Sys.Date(), "%Y%m%d"),
                                  ".csv")))






