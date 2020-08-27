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

    mer_psnu %>% distinct(operatingunit, fiscal_year)

    mer_psnu %>%
        distinct(fiscal_year, fundingagency) %>%
        arrange(fundingagency, fiscal_year) %>%
        prinf()

    mer_psnu %>%
        distinct(modality) %>%
        arrange(fundingagency, fiscal_year) %>%
        prinf()

    mer_psnu <- mer_psnu %>%
        mutate(fundingagency = str_replace(fundingagency, "HHS/", ""))

    ## Indicators

    mer_psnu %>%
        distinct(indicatortype, indicator) %>%
        arrange(indicator) %>%
        prinf()

    ## Data - Testing

    mer_psnu %>%
        filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "HTS_TST_NEG", "HTS_INDEX")) %>%
        distinct(indicator, standardizeddisaggregate, otherdisaggregate) %>%
        arrange(indicator, standardizeddisaggregate) %>%
        prinf()

    mer_psnu %>%
        filter(indicator == "HTS_INDEX") %>%
        distinct(modality) %>%
        arrange(modality) %>%
        prinf()

    ## Index Cases - All

    ## OU
    df_ou_idx_tst <- mer_ou %>%
        select(
            fiscal_year, fundingagency, primepartner, mech_code:mech_name,
            indicator, standardizeddisaggregate,
            trendscoarse, sex, modality, targets:cumulative
        ) %>%
        filter(
            fiscal_year == 2020,
            indicator == 'HTS_INDEX',
            standardizeddisaggregate %in% c(
                "1:Age/Sex/IndexCasesOffered",
                "2:Age/Sex/IndexCasesAccepted",
                "3:Age Aggregated/Sex/Contacts"
            )
        ) %>%
        rowwise() %>%
        mutate(
            standardizeddisaggregate = str_split(standardizeddisaggregate, "/") %>%
                unlist() %>%
                last(),
        ) %>%
        ungroup() %>%
        group_by(
            fiscal_year, fundingagency, primepartner,
            mech_code, mech_name, indicator, standardizeddisaggregate,
            trendscoarse, sex, modality
        ) %>%
        summarise_at(vars(qtr1:qtr3), sum, na.rm = TRUE) %>%
        ungroup() %>%
        gather(key = reporting_period, value = measurement, qtr1:qtr3) %>%
        spread(standardizeddisaggregate, measurement) %>%
        mutate(
            IndexCasesNotAccepted = ifelse(
                !is.na(IndexCasesAccepted) & !is.na(IndexCasesOffered),
                IndexCasesOffered - IndexCasesAccepted,
                NA
            ),
            reporting_period = str_remove(toupper(reporting_period), "TR")
        ) %>%
        gather(key = standardizeddisaggregate, value = measurement, Contacts:IndexCasesNotAccepted) %>%
        mutate(
            standardizeddisaggregate = case_when(
                standardizeddisaggregate == "Contacts" ~ "Contacts Elicited",
                standardizeddisaggregate == "IndexCasesNotAccepted" ~ "Index Cases Not-accepted",
                TRUE ~ str_replace(standardizeddisaggregate, "IndexCases", "Index Cases ")
            ),
            standardizeddisaggregate = factor(
                standardizeddisaggregate,
                levels = c(
                    "Index Cases Offered",
                    "Index Cases Accepted",
                    "Index Cases Not-accepted",
                    "Contacts Elicited"
                )
            ),
        ) %>%
        relocate(standardizeddisaggregate, .after = indicator)

    df_ou_idx_tst %>% glimpse()

    ## PSNU
    df_psnu_idx_tst <- mer_psnu %>%
        select(
            fiscal_year, fundingagency, primepartner, snu1:psnuuid, mech_code:mech_name,
            indicator, standardizeddisaggregate,
            trendscoarse, sex, modality, targets:cumulative
        ) %>%
        filter(
            fiscal_year == 2020,
            indicator == 'HTS_INDEX',
            standardizeddisaggregate %in% c(
                "1:Age/Sex/IndexCasesOffered",
                "2:Age/Sex/IndexCasesAccepted",
                "3:Age Aggregated/Sex/Contacts"
            )
        ) %>%
        rowwise() %>%
        mutate(
            standardizeddisaggregate = str_split(standardizeddisaggregate, "/") %>% unlist() %>% last(),
        ) %>%
        ungroup() %>%
        group_by(
            fiscal_year, fundingagency, primepartner,
            snu1, snu1uid, psnu, psnuuid, mech_code, mech_name,
            indicator, standardizeddisaggregate,
            trendscoarse, sex, modality
        ) %>%
        summarise_at(vars(qtr1:qtr3), sum, na.rm = TRUE) %>%
        ungroup() %>%
        gather(key = reporting_period, value = measurement, qtr1:qtr3) %>%
        spread(standardizeddisaggregate, measurement) %>%
        mutate(
            IndexCasesNotAccepted = ifelse(
                !is.na(IndexCasesAccepted) & !is.na(IndexCasesOffered),
                IndexCasesOffered - IndexCasesAccepted,
                NA
            ),
            reporting_period = str_remove(toupper(reporting_period), "TR")
        ) %>%
        gather(key = standardizeddisaggregate, value = measurement, Contacts:IndexCasesNotAccepted) %>%
        mutate(
            standardizeddisaggregate = case_when(
                standardizeddisaggregate == "Contacts" ~ "Contacts Elicited",
                standardizeddisaggregate == "IndexCasesNotAccepted" ~ "Index Cases Not-accepted",
                TRUE ~ str_replace(standardizeddisaggregate, "IndexCases", "Index Cases ")
            ),
            standardizeddisaggregate = factor(
                standardizeddisaggregate,
                levels = c(
                    "Index Cases Offered",
                    "Index Cases Accepted",
                    "Index Cases Not-accepted",
                    "Contacts Elicited"
                )
            ),
        ) %>%
        relocate(standardizeddisaggregate, .after = indicator)

    df_psnu_idx_tst %>% glimpse()
    df_psnu_idx_tst %>% View(title = "IDX Testing")


    ## Index Testing Results & Yield

    ## OU
    df_ou_idx_rst <- mer_ou %>%
        select(
            fiscal_year, fundingagency, primepartner, mech_code:mech_name,
            indicator, standardizeddisaggregate, otherdisaggregate,
            trendscoarse, sex, statushiv, modality, qtr1:qtr3
        ) %>%
        filter(
            fiscal_year == 2020,
            indicator == 'HTS_INDEX',
            standardizeddisaggregate == "4:Age/Sex/Result"
        ) %>%
        mutate(
            standardizeddisaggregate = case_when(
                .$otherdisaggregate == "Known at Entry" ~ "HTS_INDEX_KNOWNPOS",
                .$otherdisaggregate == "Newly Identified" & .$statushiv == "Positive" ~ "HTS_INDEX_NEWPOS",
                .$otherdisaggregate == "Newly Identified" & .$statushiv == "Negative" ~ "HTS_INDEX_NEWNEG",
                TRUE ~ standardizeddisaggregate
            ),
            modality = case_when(
                modality == "Index" ~ "Facility",
                modality == "IndexMod" ~ "Community"
            )
        ) %>%
        select(-c(otherdisaggregate, statushiv)) %>%
        group_by_at(vars(-c(qtr1:qtr3))) %>%
        summarise_at(vars(qtr1:qtr3), sum, na.rm = TRUE) %>%
        ungroup() %>%
        gather(key = reporting_period, value = measurement, qtr1:qtr3) %>%
        spread(standardizeddisaggregate, measurement) %>%
        mutate(
            reporting_period = str_remove(toupper(reporting_period), "TR"),
            HTS_INDEX_YIELD = HTS_INDEX_NEWPOS / (HTS_INDEX_NEWPOS + HTS_INDEX_NEWNEG),
            HTS_INDEX_YIELD = ifelse(!is.nan(HTS_INDEX_YIELD), HTS_INDEX_YIELD, NA)
        ) %>%
        gather(key = standardizeddisaggregate, value = measurement, HTS_INDEX_KNOWNPOS:HTS_INDEX_YIELD) %>%
        relocate(standardizeddisaggregate, .after = indicator)


    df_ou_idx_rst %>% glimpse()
    df_ou_idx_rst %>% View(title = "IDX Results")

    ## PNSU
    df_psnu_idx_rst <- mer_psnu %>%
        select(
            fiscal_year, fundingagency, primepartner, snu1:psnuuid, mech_code:mech_name,
            indicator, standardizeddisaggregate, otherdisaggregate,
            trendscoarse, sex, statushiv, modality, qtr1:qtr3
        ) %>%
        filter(
            fiscal_year == 2020,
            indicator == 'HTS_INDEX',
            standardizeddisaggregate == "4:Age/Sex/Result"
        ) %>%
        mutate(
            standardizeddisaggregate = case_when(
                .$otherdisaggregate == "Known at Entry" ~ "HTS_INDEX_KNOWNPOS",
                .$otherdisaggregate == "Newly Identified" & .$statushiv == "Positive" ~ "HTS_INDEX_NEWPOS",
                .$otherdisaggregate == "Newly Identified" & .$statushiv == "Negative" ~ "HTS_INDEX_NEWNEG",
                TRUE ~ standardizeddisaggregate
            ),
            modality = case_when(
                modality == "Index" ~ "Facility",
                modality == "IndexMod" ~ "Community"
            )
        ) %>%
        select(-c(otherdisaggregate, statushiv)) %>%
        group_by_at(vars(-c(qtr1:qtr3))) %>%
        summarise_at(vars(qtr1:qtr3), sum, na.rm = TRUE) %>%
        ungroup() %>%
        gather(key = reporting_period, value = measurement, qtr1:qtr3) %>%
        spread(standardizeddisaggregate, measurement) %>%
        mutate(
            reporting_period = str_remove(toupper(reporting_period), "TR"),
            HTS_INDEX_YIELD = HTS_INDEX_NEWPOS / (HTS_INDEX_NEWPOS + HTS_INDEX_NEWNEG),
            HTS_INDEX_YIELD = ifelse(!is.nan(HTS_INDEX_YIELD), HTS_INDEX_YIELD, NA)
        ) %>%
        gather(key = standardizeddisaggregate, value = measurement, HTS_INDEX_KNOWNPOS:HTS_INDEX_YIELD) %>%
        relocate(standardizeddisaggregate, .after = indicator)


    df_psnu_idx_rst %>% glimpse()
    df_psnu_idx_rst %>% View(title = "IDX Results")

    ## Merge Testing & Index

    ## OU
    df_ou_idx_tst <- df_ou_idx_tst %>%
        bind_rows(df_ou_idx_rst)

    df_ou_idx_tst %>% glimpse()
    df_ou_idx_tst %>% distinct(standardizeddisaggregate)

    ## PSNU
    df_psnu_idx_tst <- df_psnu_idx_tst %>%
        bind_rows(df_psnu_idx_rst)

    df_psnu_idx_tst %>% glimpse()
    df_psnu_idx_tst %>% distinct(standardizeddisaggregate)

## VIZ --------------------------------------------------

    ## INDEX CASES

    df_ou_idx_cases <- df_ou_idx_tst %>%
        filter(trendscoarse == "15+", !str_detect(standardizeddisaggregate, "HTS")) %>%
        mutate(
            standardizeddisaggregate = factor(
                standardizeddisaggregate,
                levels = c(
                    "Index Cases Offered",
                    "Index Cases Accepted",
                    "Index Cases Not-accepted",
                    "Contacts Elicited"
                )
            )
        ) %>%
        group_by(fundingagency, standardizeddisaggregate, reporting_period) %>%
        summarise(measurement = sum(measurement, na.rm = TRUE)) %>%
        ungroup()

    ## INDEX TESTS
    df_ou_idx_tests <- df_ou_idx_tst %>%
        filter(trendscoarse == "15+", str_detect(standardizeddisaggregate, "HTS")) %>%
        group_by(fundingagency, standardizeddisaggregate, reporting_period) %>%
        summarise(measurement = sum(measurement, na.rm = TRUE)) %>%
        ungroup()

    ## INDEX Summary

    ## OU
    df_ou_idx <- df_ou_idx_tst %>%
        filter(
            trendscoarse == "<15",
            standardizeddisaggregate %in% c(
                "Index Cases Accepted",
                "Contacts Elicited",
                "HTS_INDEX_NEWPOS",
                "HTS_INDEX_YIELD"
            )
        ) %>%
        group_by(standardizeddisaggregate, reporting_period) %>%
        summarise(measurement = sum(measurement, na.rm = TRUE)) %>%
        ungroup()

    df_ou_idx %>% View()

    df_ou_idx_1 <- df_ou_idx_tst %>%
        filter(
            trendscoarse == "<15",
            standardizeddisaggregate %in% c(
                "Index Cases Accepted",
                "Contacts Elicited",
                "HTS_INDEX_NEWPOS"
            )
        ) %>%
        group_by(standardizeddisaggregate) %>%
        summarise(measurement = sum(measurement, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(
            standardizeddisaggregate = factor(
                standardizeddisaggregate,
                levels = c(
                    "Index Cases Accepted",
                    "Contacts Elicited",
                    "HTS_INDEX_NEWPOS"
                ),
                labels = c(
                    "Index Cases Accepted",
                    "Contacts Elicited",
                    "Contacts New POS"
                )
            )
        )

    df_ou_idx_2 <- df_ou_idx_tst %>%
        filter(
            trendscoarse == "<15",
            standardizeddisaggregate == "HTS_INDEX_YIELD"
        ) %>%
        group_by(standardizeddisaggregate) %>%
        summarise(measurement = sum(measurement, na.rm = TRUE)) %>%
        ungroup()


    ggplot(data = df_ou_idx_1, aes(standardizeddisaggregate, measurement, fill = standardizeddisaggregate)) +
        geom_col(show.legend = T) +
        geom_label(aes(standardizeddisaggregate, measurement, label = comma(measurement)),
                  position = position_identity(), color = "white", fontface = "bold", show.legend = F) +
        scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086")) +
        labs(
            title = "DRC - Index Cases, Contacts & Testing Results",
            subtitle = "FY20 cumulative <15 results and grouped at OU level.\nNational Index Testing Yield is at 9.61%",
            caption = graph_note
        ) +
        coord_flip() +
        si_style_nolines() +
        theme(
            legend.position = "top",
            legend.title = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_blank()
        )

    si_save("FY20_IndexTesting_Peds", path = dir_graphics)

    ## Index Testing by Agency
    df_ou_idx_a <- df_ou_idx_tst %>%
        filter(
            trendscoarse == "<15",
            standardizeddisaggregate %in% c(
                "Index Cases Accepted",
                "Contacts Elicited",
                "HTS_INDEX_NEWPOS",
                "HTS_INDEX_YIELD"
            )
        ) %>%
        group_by(fundingagency, standardizeddisaggregate, reporting_period) %>%
        summarise(measurement = sum(measurement, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(fundingagency = factor(fundingagency, levels = c("USAID", "CDC", "DOD")))

    df_ou_idx_a1 <- df_ou_idx_a %>%
        group_by(fundingagency, standardizeddisaggregate) %>%
        summarise(measurement = sum(measurement, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(standardizeddisaggregate != "HTS_INDEX_YIELD") %>%
        mutate(
            standardizeddisaggregate = factor(
                standardizeddisaggregate,
                levels = c(
                    "Index Cases Accepted",
                    "Contacts Elicited",
                    "HTS_INDEX_NEWPOS"
                ),
                labels = c(
                    "Index Cases Accepted",
                    "Contacts Elicited",
                    "Contacts New POS"
                )
            )
        )

    df_ou_idx_a2 <- df_ou_idx_a %>%
        group_by(fundingagency, standardizeddisaggregate) %>%
        summarise(measurement = sum(measurement, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(standardizeddisaggregate == "HTS_INDEX_YIELD")

    ggplot() +
        geom_col(data = df_ou_idx_a1,
                 aes(fundingagency, measurement, fill = standardizeddisaggregate),
                 position = "dodge", show.legend = T) +
        geom_text(data = df_ou_idx_a1,
                  aes(fundingagency, 100, label = comma(measurement), group = standardizeddisaggregate),
                  position = position_dodge(width = 1), vjust = -.5, color = grey80k, show.legend = F) +
        geom_point(data = df_ou_idx_a2,
                   aes(x = fundingagency, y = measurement * 1000),
                   shape = 21, size = 10, color = si_lorange, fill = "white") +
        geom_text(data = df_ou_idx_a2,
                  aes(fundingagency, measurement * 1000, label = paste0(round(measurement, 1), "%"), group = standardizeddisaggregate),
                  position = position_dodge(width = 1), hjust = .5, vjust = .5, color = grey80k, size = 3, show.legend = F) +
        scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086")) +
        scale_y_continuous(
            name = "Cases | Contacts | New POS",
            sec.axis = sec_axis(trans = ~./1000, name = "Index Yield (%)")
        ) +
        labs(
            title = "DRC - Index Cases, Contacts & Testing Results",
            subtitle = "FY20 cumulative <15 results and grouped at OU level.\nWhite circles represent the Index Testing Yield",
            caption = graph_note,
            x = "",
            y = ""
        ) +
        si_style_nolines() +
        theme(
            legend.position = "top",
            legend.title = element_blank(),
            axis.text.y = element_blank(),
            axis.title = element_blank()
        )

    si_save("FY20_IndexTesting_Peds_by_agency", path = dir_graphics)



    ## Index Testing by USAID/SNU1
    df_psnu_idx_s <- df_psnu_idx_tst %>%
        filter(
            fundingagency == "USAID",
            trendscoarse == "<15",
            standardizeddisaggregate %in% c(
                "Index Cases Accepted",
                "Contacts Elicited",
                "HTS_INDEX_NEWPOS",
                "HTS_INDEX_YIELD"
            )
        ) %>%
        group_by(snu1, standardizeddisaggregate) %>%
        summarise(measurement = sum(measurement, na.rm = TRUE)) %>%
        ungroup()

    df_psnu_idx_s1 <- df_psnu_idx_s %>%
        filter(standardizeddisaggregate != "HTS_INDEX_YIELD") %>%
        mutate(
            standardizeddisaggregate = factor(
                standardizeddisaggregate,
                levels = c(
                    "Index Cases Accepted",
                    "Contacts Elicited",
                    "HTS_INDEX_NEWPOS"
                ),
                labels = c(
                    "Index Cases Accepted",
                    "Contacts Elicited",
                    "Contacts New POS"
                )
            )
        )

    df_psnu_idx_s2 <- df_psnu_idx_s %>%
        filter(standardizeddisaggregate == "HTS_INDEX_YIELD")

    ggplot() +
        geom_col(data = df_psnu_idx_s1,
                 aes(snu1, measurement, fill = standardizeddisaggregate),
                 position = "dodge", show.legend = T) +
        geom_text(data = df_psnu_idx_s1,
                  aes(snu1, 100, label = comma(measurement), group = standardizeddisaggregate),
                  position = position_dodge(width = 1), vjust = -.5, color = grey80k, show.legend = F) +
        geom_point(data = df_psnu_idx_s2,
                   aes(x = snu1, y = measurement * 200),
                   shape = 21, size = 10, color = si_lorange, fill = "white") +
        geom_text(data = df_psnu_idx_s2,
                  aes(snu1, measurement * 200, label = paste0(round(measurement, 1), "%"), group = standardizeddisaggregate),
                  position = position_dodge(width = 1), hjust = .5, vjust = .5, color = grey80k, size = 3, show.legend = F) +
        scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086")) +
        scale_y_continuous(
            name = "Cases | Contacts | New POS",
            sec.axis = sec_axis(trans = ~./200, name = "Index Yield (%)")
        ) +
        labs(
            title = "DRC - Index Cases, Contacts & Testing Results",
            subtitle = "FY20 USAID/SNU1 cumulative <15 results.\nWhite circles represent the Index Testing Yield",
            caption = graph_note,
            x = "",
            y = ""
        ) +
        si_style_nolines() +
        theme(
            legend.position = "top",
            legend.title = element_blank(),
            axis.text.y = element_blank(),
            axis.title = element_blank()
        )

    si_save("FY20_IndexTesting_Peds_USAID_SNU1", path = dir_graphics)


    ## Index Testing USAID/IM
    df_psnu_idx_im <- df_psnu_idx_tst %>%
        filter(
            fundingagency == "USAID",
            trendscoarse == "<15",
            standardizeddisaggregate %in% c(
                "Index Cases Accepted",
                "Contacts Elicited",
                "HTS_INDEX_NEWPOS",
                "HTS_INDEX_YIELD"
            )
        ) %>%
        group_by(mech_name, standardizeddisaggregate) %>%
        summarise(measurement = sum(measurement, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(
            mech_name = case_when(
                str_detect(mech_name, "Epidemic") ~ "HEC",
                str_detect(mech_name, "Haut-K") ~ "IHAP-HK",
                str_detect(mech_name, "Kinshasa") ~ "IHAP-KIN",
                TRUE ~ mech_name
            )
        )

    df_psnu_idx_im1 <- df_psnu_idx_im %>%
        filter(standardizeddisaggregate != "HTS_INDEX_YIELD") %>%
        mutate(
            standardizeddisaggregate = factor(
                standardizeddisaggregate,
                levels = c(
                    "Index Cases Accepted",
                    "Contacts Elicited",
                    "HTS_INDEX_NEWPOS"
                ),
                labels = c(
                    "Index Cases Accepted",
                    "Contacts Elicited",
                    "Contacts New POS"
                )
            )
        )

    df_psnu_idx_im2 <- df_psnu_idx_im %>%
        filter(standardizeddisaggregate == "HTS_INDEX_YIELD")


    ggplot() +
        geom_col(data = df_psnu_idx_im1,
                 aes(mech_name, measurement, fill = standardizeddisaggregate),
                 position = "dodge", show.legend = T) +
        geom_text(data = df_psnu_idx_im1,
                  aes(mech_name, 100, label = comma(measurement), group = standardizeddisaggregate),
                  position = position_dodge(width = 1), vjust = -.5, color = grey80k, show.legend = F) +
        geom_point(data = df_psnu_idx_im2,
                   aes(x = mech_name, y = measurement * 200),
                   shape = 21, size = 10, color = si_lorange, fill = "white") +
        geom_text(data = df_psnu_idx_im2,
                  aes(mech_name, measurement * 200, label = paste0(round(measurement, 1), "%"), group = standardizeddisaggregate),
                  position = position_dodge(width = 1), hjust = .5, vjust = .5, color = grey80k, size = 3, show.legend = F) +
        scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086")) +
        scale_y_continuous(
            name = "Cases | Contacts | New POS",
            sec.axis = sec_axis(trans = ~./200, name = "Index Yield (%)")
        ) +
        labs(
            title = "DRC - Index Cases, Contacts & Testing Results",
            subtitle = "FY20 USAID/IM cumulative <15 results.\nWhite circles represent the Index Testing Yield",
            caption = graph_note,
            x = "",
            y = ""
        ) +
        si_style_nolines() +
        theme(
            legend.position = "top",
            legend.title = element_blank(),
            axis.text.y = element_blank(),
            axis.title = element_blank()
        )

    si_save("FY20_IndexTesting_peds_USAID_IM", path = dir_graphics)



    ## Index Testing USAID/IM/Q
    df_psnu_idx_imq <- df_psnu_idx_tst %>%
        filter(
            fundingagency == "USAID",
            trendscoarse == "<15",
            standardizeddisaggregate %in% c(
                "Index Cases Accepted",
                "Contacts Elicited",
                "HTS_INDEX_NEWPOS",
                "HTS_INDEX_YIELD"
            )
        ) %>%
        group_by(mech_name, standardizeddisaggregate, reporting_period) %>%
        summarise(measurement = sum(measurement, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(
            mech_name = case_when(
                str_detect(mech_name, "Epidemic") ~ "HEC",
                str_detect(mech_name, "Haut-K") ~ "IHAP-HK",
                str_detect(mech_name, "Kinshasa") ~ "IHAP-KIN",
                TRUE ~ mech_name
            )
        )

    df_psnu_idx_imq1 <- df_psnu_idx_imq %>%
        filter(standardizeddisaggregate != "HTS_INDEX_YIELD") %>%
        mutate(
            standardizeddisaggregate = factor(
                standardizeddisaggregate,
                levels = c(
                    "Index Cases Accepted",
                    "Contacts Elicited",
                    "HTS_INDEX_NEWPOS"
                ),
                labels = c(
                    "Index Cases Accepted",
                    "Contacts Elicited",
                    "Contacts New POS"
                )
            )
        )

    df_psnu_idx_imq2 <- df_psnu_idx_imq %>%
        filter(standardizeddisaggregate == "HTS_INDEX_YIELD")


    ggplot() +
        geom_col(data = df_psnu_idx_imq1,
                 aes(reporting_period, measurement, fill = standardizeddisaggregate),
                 position = "dodge", show.legend = T) +
        geom_text(data = df_psnu_idx_imq1,
                  aes(reporting_period, 100, label = comma(round(measurement, 0)), group = standardizeddisaggregate),
                  position = position_dodge(width = 1), hjust = 0, size = 3, color = grey80k, show.legend = F) +
        geom_point(data = df_psnu_idx_imq2,
                   aes(x = reporting_period, y = measurement * 500),
                   shape = 21, size = 10, color = si_lorange, fill = "white") +
        geom_text(data = df_psnu_idx_imq2,
                  aes(reporting_period, measurement * 500, label = paste0(round(measurement, 1), "%"), group = standardizeddisaggregate),
                  position = position_dodge(width = 1), hjust = .5, vjust = .5, color = grey80k, size = 3, show.legend = F) +
        scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086")) +
        scale_y_continuous(
            name = "Cases | Contacts | New POS",
            sec.axis = sec_axis(trans = ~./500, name = "Index Yield (%)")
        ) +
        facet_wrap(~mech_name) +
        coord_flip() +
        labs(
            title = "DRC - Index Cases, Contacts & Testing Results",
            subtitle = "FY20 USAID/IM cumulative <15 results.\nWhite circles represent the Index Testing Yield",
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

    si_save("FY20_IndexTesting_Peds_USAID_IM_Qtr", path = dir_graphics)




























