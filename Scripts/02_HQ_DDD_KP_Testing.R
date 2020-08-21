## PROJECT:  SI Support for DRC
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Process Data submitted by IPs
## Date:     2020-08-21

# LIBRARIES -------------------------------

library(tidyverse)
library(readxl)
library(vroom)
library(glamr)
library(ICPIutilities)
library(janitor)
library(here)

# CONFIGS --------------------------------

source("./Scripts/00_Config.R")

# GLABALS --------------------------------

    ## DRC Data folder
    dir_drc <- "../../PEPFAR/COUNTRIES/DRC/Data"

    ## Genie daily extract - 20200814
    file_sites <- "Genie-SiteByIMs-Democratic-Republic-of-the-Congo-Daily-2020-08-21.zip"
    file_psnu <- "Genie-PSNUByIMs-Democratic-Republic-of-the-Congo-Daily-2020-08-21.zip"

# DATA ------------------------------------

    ## MER Sites Pre-Cleaned data
    mer_sites <- read_msd(file = here(dir_drc, file_sites))

    mer_sites %>% glimpse()

    mer_sites %>%
        distinct(operatingunit, fiscal_year)


    ## MER PSNU Pre-Cleaned data
    mer_psnu <- read_msd(file = here(dir_drc, file_psnu))

    mer_psnu %>% glimpse()

    ## Fiscal Years
    mer_psnu %>% distinct(operatingunit, fiscal_year)

    ## Fiscal Years & Agencies
    mer_psnu %>% distinct(operatingunit, fiscal_year, fundingagency)

    ## Indicators
    mer_psnu %>%
        distinct(indicatortype, indicator) %>%
        arrange(indicator) %>%
        prinf()




    ## KP Cascade

    mer_psnu %>%
        filter(indicator %in% c('HTS_TST', 'HTS_TST_POS')) %>%
        distinct(disaggregate, standardizeddisaggregate) %>%
        arrange(standardizeddisaggregate)

    mer_psnu %>%
        filter(
            fiscal_year == 2020,
            indicator == 'HTS_TST',
            standardizeddisaggregate == "KeyPop/Result"
        ) %>% View()
        group_by(fundingagency) %>%
        summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)






