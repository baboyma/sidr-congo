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

# DATA ------------------------------------

    ## MER Sites Pre-Cleaned data

    # mer_sites <- read_msd(file = here(dir_drc, file_sites))
    # mer_sites %>% glimpse()
    # mer_sites %>% distinct(operatingunit, fiscal_year)


    ## MER OU

    mer_ou <- read_msd(file = here(dir_merdata, file_ou))

    mer_ou <- mer_ou %>%
        filter(operatingunit == curr_ou) %>%
        mutate(fundingagency = str_remove(fundingagency, "HHS/"))

    mer_ou %>% glimpse()
    mer_ou %>% distinct(operatingunit, fiscal_year)

    ## MER PSNU Pre-Cleaned data

    mer_psnu <- read_msd(file = here(dir_drc, file_psnu))

    mer_psnu %>% glimpse()

    mer_psnu <- mer_psnu %>%
        mutate(fundingagency = str_replace(fundingagency, "HHS/", ""))

    mer_psnu %>% distinct(operatingunit, fiscal_year)

    mer_psnu %>%
        distinct(fiscal_year, fundingagency) %>%
        arrange(fundingagency, fiscal_year) %>%
        prinf()

    mer_psnu %>%
        distinct(modality) %>%
        arrange(modality) %>%
        prinf()

    ## Indicators

    mer_psnu %>%
        distinct(indicatortype, indicator) %>%
        arrange(indicator) %>%
        prinf()

    ## Data - Testing

    mer_psnu %>%
        filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "HTS_TST_NEG")) %>%
        distinct(indicator, standardizeddisaggregate, otherdisaggregate) %>%
        arrange(indicator, standardizeddisaggregate) %>%
        prinf()


    ## HTS KP

    ## PSNU
    df_psnu_kp_tst <- mer_psnu %>%
        filter(
            fiscal_year == 2020,
            indicator %in% c('HTS_TST','HTS_TST_POS','HTS_TST_NEG'),
            standardizeddisaggregate == "KeyPop/Result"
        ) %>%
        select(
            fundingagency, operatingunit, snu1, psnu, indicator,
            mech_name, otherdisaggregate, qtr1:qtr3
        ) %>%
        gather(metrics, measurement, qtr1:qtr3)

    df_psnu_kp_tst

    df_psnu_kp_tst_y <- df_psnu_kp_tst %>%
        filter(!str_detect(indicator, "_NEG")) %>%
        group_by_at(vars(-c(indicator, measurement))) %>%
        summarise(
            HTS_TST_YIELD = measurement[indicator == 'HTS_TST_POS'] / measurement[indicator == 'HTS_TST']
        ) %>%
        gather(indicator, measurement, HTS_TST_YIELD) %>%
        bind_rows(df_psnu_kp_tst)

    # SNU1
    df_psnu_kp_tst_s <- df_psnu_kp_tst %>%
        group_by(fundingagency, snu1, indicator) %>%
        summarise_at(vars(measurement), sum, na.rm =T)

    df_psnu_kp_tst_s_y <- df_psnu_kp_tst_s %>%
        filter(!str_detect(indicator, "_NEG")) %>%
        group_by(fundingagency, snu1) %>%
        summarise(
            HTS_TST_YIELD = measurement[indicator == 'HTS_TST_POS'] / measurement[indicator == 'HTS_TST']
        ) %>%
        gather(indicator, measurement, HTS_TST_YIELD) %>%
        bind_rows(df_psnu_kp_tst_s)

    # Agency / IM
    df_psnu_kp_tst_im <- df_psnu_kp_tst %>%
        group_by(fundingagency, mech_name, indicator) %>%
        summarise_at(vars(measurement), sum, na.rm =T)

    df_psnu_kp_tst_im_y <- df_psnu_kp_tst_im %>%
        filter(!str_detect(indicator, "_NEG")) %>%
        group_by(fundingagency, mech_name) %>%
        summarise(
            HTS_TST_YIELD = measurement[indicator == 'HTS_TST_POS'] / measurement[indicator == 'HTS_TST']
        ) %>%
        gather(indicator, measurement, HTS_TST_YIELD) %>%
        bind_rows(df_psnu_kp_tst_im) %>%
        mutate(
            mech_name  = case_when(
                str_detect(mech_name, "Epidemic") ~ "HEC",
                str_detect(mech_name, "Haut-K") ~ "IHAP-HK",
                str_detect(mech_name, "Kinshasa") ~ "IHAP-KIN",
                str_detect(mech_name, "Increase Access") ~ "ICAP",
                TRUE ~ mech_name
            )
        )

    # Agency
    df_psnu_kp_tst_a <- df_psnu_kp_tst %>%
        group_by(fundingagency, indicator) %>%
        summarise_at(vars(measurement), sum, na.rm =T)

    df_psnu_kp_tst_a_y <- df_psnu_kp_tst_a %>%
        filter(!str_detect(indicator, "_NEG")) %>%
        group_by(fundingagency) %>%
        summarise(
            HTS_TST_YIELD = measurement[indicator == 'HTS_TST_POS'] / measurement[indicator == 'HTS_TST']
        ) %>%
        gather(indicator, measurement, HTS_TST_YIELD) %>%
        bind_rows(df_psnu_kp_tst_a)


# VIZ ------------------

    # Agency
    ggplot() +
        geom_col(data = df_psnu_kp_tst_a_y %>% filter(indicator != "HTS_TST_YIELD"),
                 aes(fundingagency, measurement, fill = indicator),
                 position = "dodge", show.legend = T) +
        geom_text(data = df_psnu_kp_tst_a_y %>% filter(indicator != "HTS_TST_YIELD"),
                  aes(fundingagency, 100, label = comma(round(measurement, 0)), group = indicator),
                  position = position_dodge(width = 1), hjust = 0, size = 3, color = grey80k, show.legend = F) +
        geom_point(data = df_psnu_kp_tst_a_y %>% filter(indicator == "HTS_TST_YIELD"),
                   aes(x = fundingagency, y = measurement * 20000),
                   shape = 21, size = 10, color = si_lorange, fill = "white") +
        geom_text(data = df_psnu_kp_tst_a_y %>% filter(indicator == "HTS_TST_YIELD"),
                  aes(fundingagency, measurement * 20000, label = percent(round(measurement, 2)), group = indicator),
                  position = position_dodge(width = 1), hjust = .5, vjust = .5, color = grey80k, size = 3, show.legend = F) +
        scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086")) +
        scale_y_continuous(
            name = "Cases | Contacts | New POS",
            sec.axis = sec_axis(trans = ~./20000, name = "Index Yield (%)")
        ) +
        coord_flip() +
        labs(
            title = "DRC - KP Testing Yield",
            subtitle = "FY20 cumulative results by Agency.\nWhite circles represent % Yield",
            caption = graph_note,
            x = "",
            y = ""
        ) +
        si_style_nolines() +
        theme(
            legend.position = "top",
            legend.title = element_blank(),
            axis.text.x = element_blank(),
            axis.title = element_blank()
        )

    si_save("FY20_KP_TestingYield_Agency", path = dir_graphics)



    # agency / SNU1
    ggplot() +
        geom_col(data = df_psnu_kp_tst_s_y %>% filter(indicator != "HTS_TST_YIELD"),
                 aes(snu1, measurement, fill = indicator),
                 position = "dodge", show.legend = T) +
        geom_text(data = df_psnu_kp_tst_s_y %>% filter(indicator != "HTS_TST_YIELD"),
                  aes(snu1, 100, label = comma(round(measurement, 0)), group = indicator),
                  position = position_dodge(width = 1), hjust = 0, size = 3, color = grey80k, show.legend = F) +
        geom_point(data = df_psnu_kp_tst_s_y %>% filter(indicator == "HTS_TST_YIELD"),
                   aes(snu1, measurement * 20000),
                   shape = 21, size = 10, color = si_lorange, fill = "white") +
        geom_text(data = df_psnu_kp_tst_s_y %>% filter(indicator == "HTS_TST_YIELD"),
                  aes(snu1, measurement * 20000, label = percent(measurement, 1), group = indicator),
                  position = position_dodge(width = 1), hjust = .5, vjust = .5, color = grey80k, size = 3, show.legend = F) +
        scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086")) +
        scale_y_continuous(
            name = "Cases | Contacts | New POS",
            sec.axis = sec_axis(trans = ~./20000, name = "Index Yield (%)")
        ) +
        coord_flip() +
        facet_wrap(~fundingagency) +
        labs(
            title = "DRC - KP Testing Yield",
            subtitle = "FY20 cumulative results by SNU\nWhite circles represent % Yield",
            caption = graph_note,
            x = "",
            y = ""
        ) +
        si_style_nolines() +
        theme(
            legend.position = "top",
            legend.title = element_blank(),
            axis.text.x = element_blank(),
            axis.title = element_blank()
        )

    si_save("FY20_KP_TestingYield_AgencySNU1", path = dir_graphics)



    # agency / IM
    ggplot() +
        geom_col(data = df_psnu_kp_tst_im_y %>% filter(indicator != "HTS_TST_YIELD"),
                 aes(fundingagency, measurement, fill = indicator),
                 position = "dodge", show.legend = T) +
        geom_text(data = df_psnu_kp_tst_im_y %>% filter(indicator != "HTS_TST_YIELD"),
                  aes(fundingagency, 100, label = comma(round(measurement, 0)), group = indicator),
                  position = position_dodge(width = 1), hjust = 0, size = 3, color = grey80k, show.legend = F) +
        geom_point(data = df_psnu_kp_tst_im_y %>% filter(indicator == "HTS_TST_YIELD"),
                   aes(fundingagency, measurement * 20000),
                   shape = 21, size = 10, color = si_lorange, fill = "white") +
        geom_text(data = df_psnu_kp_tst_im_y %>% filter(indicator == "HTS_TST_YIELD"),
                  aes(fundingagency, measurement * 20000, label = percent(measurement, 1), group = indicator),
                  position = position_dodge(width = 1), hjust = .5, vjust = .5, color = grey80k, size = 3, show.legend = F) +
        scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086")) +
        scale_y_continuous(
            name = "Cases | Contacts | New POS",
            sec.axis = sec_axis(trans = ~./20000, name = "Index Yield (%)")
        ) +
        coord_flip() +
        facet_wrap(~mech_name) +
        labs(
            title = "DRC - KP Testing Yield",
            subtitle = "FY20 cumulative results by SNU\nWhite circles represent % Yield",
            caption = graph_note,
            x = "",
            y = ""
        ) +
        si_style_nolines() +
        theme(
            legend.position = "top",
            legend.title = element_blank(),
            axis.text.x = element_blank(),
            axis.title = element_blank()
        )

    si_save("FY20_KP_TestingYield_AgencyIM", path = dir_graphics)
