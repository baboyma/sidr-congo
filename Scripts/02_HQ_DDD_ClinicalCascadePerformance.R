## PROJECT:  SI Support for DRC
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  DRC - Clinical Cascade Performance
## Date:     2020-08-28

# LIBRARIES -------------------------------

library(tidyverse)
library(readxl)
library(vroom)
library(glamr)
library(janitor)
library(here)
library(glitr)
library(scales)
library(ICPIutilities)

# CONFIGS --------------------------------

source("./Scripts/00_Config.R")

# GLABALS --------------------------------

    ## DRC Data folder
    dir_drc <- "../../PEPFAR/COUNTRIES/DRC/Data"

    ## Genie daily extract - 20200814

    #file_sites <- "Genie-SiteByIMs-Democratic-Republic-of-the-Congo-Daily-2020-08-21.zip"
    #file_psnu <- "Genie-PSNUByIMs-Democratic-Republic-of-the-Congo-Daily-2020-08-21.zip"

    ## Pano data
    file_sites <- "MER_Structured_Datasets_Site_IM_FY18-20_20200814_v1_1_Democratic Republic of the Congo.zip"
    file_psnu <- "MER_Structured_Datasets_PSNU_IM_FY18-20_20200814_v1_1_Democratic Republic of the Congo.zip"
    file_ou <- "MER_Structured_Datasets_OU_IM_FY18-20_20200814_v1_1.zip"

    curr_ou = "Democratic Republic of the Congo"

    ## Foot note
    graph_note <- paste0("USAID/OHA/SIEI - HQ Q3 Data Deep Dives, ", Sys.Date())

# Data ---------------------------------------

    ## MER - OU by IM

    df_mer_ou <- read_msd(file = here(dir_merdata, file_ou))

    df_mer_ou <- df_mer_ou %>%
        filter(operatingunit == curr_ou) %>%
        mutate(fundingagency = str_remove(fundingagency, "HHS/"))

    df_mer_ou %>% glimpse()

    df_mer_ou %>% distinct(operatingunit, fiscal_year)

    ## MER - Clinical Cascade Indicators

    df_cc_ag <- df_mer_ou %>%
        filter(
            fundingagency != "Dedup",
            indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW"),
            standardizeddisaggregate %in% c("Total Numerator")
        ) %>%
        select(fiscal_year, fundingagency, indicator, targets:cumulative) %>%
        group_by(fiscal_year, fundingagency, indicator) %>%
        summarise_at(vars(targets,cumulative), sum, na.rm = TRUE) %>%
        ungroup() %>%
        mutate(achievement = ifelse(!is.na(cumulative) & !is.na(targets), cumulative / targets * 100, NA)) %>%
        gather(metrics, measurement, targets:achievement) %>%
        arrange(fiscal_year, fundingagency, indicator, metrics)

    df_cc_ag %>% View()

    ggplot() +
        geom_col(data = df_cc_ag %>% filter(metrics != "achievement"),
                 aes(metrics, measurement, fill = indicator),
                 position = position_dodge()) +
        geom_text(data = df_cc_ag %>% filter(metrics != "achievement"),
                  aes(metrics, measurement, label = measurement, group = indicator),
                  position = position_dodge(width = 1)) +
        facet_grid(fundingagency ~ fiscal_year) +
        si_style()

