## PROJECT:  SI Support for DRC
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  DRC - Clinical Cascade Performance
## Date:     2020-08-28
## Update:   2020-09-03

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

    ## Achievements Groups & Colors
    bin_qtr3 <- c("Null", "<50%", "50 - 65%", "65 - 85%", ">85%")
    bin_qtr4 <- c("Null", "<75%", "75 - 90%", "90 - 110%", ">110%")

    bin_cols <- c("#D9CDC3", "#e06714", "#f7e10f", "#7abe41", "#008000")


# FUNCTIONS ---------------------------------

    #' Dual Axis Scaling
    #'
    #' @param df data
    #' @param y1 variable name of first axis
    #' @param y2 variable name of second axis
    #' @param option scaling option: scale or trans, default = 'scale'
    #'
    get_dualaxis <- function(df,
                             y1 = "cumulative",
                             y2 = "achievement",
                             option = "scale") {

        # get ranges
        range_y1 <- range(df[[y1]], na.rm = TRUE)
        range_y2 <- range(df[[y2]], na.rm = TRUE)

        # Calculate scale factor
        scale_factor <- diff(range_y1) / diff(range_y2)

        # option == scale
        if (option == "scale") {
            return(scale_factor)
        }
        # option == trans
        else if (option == "trans") {

            trans <- ~ ((. - range_y1[1]) / scale_factor) + range_y2[1]

            return(trans)
        }
        else {
            return(NULL)
        }
    }


    #' Classify Achievements
    #'
    #' @param df data
    #' @param qtr Reporting Quarter
    #'
    group_achievements <- function(df, qtr = 3) {

        bin_qtr1 <- c("Null", "<50%", "50 - 65%", "65 - 85%", ">85%")
        bin_qtr2 <- c("Null", "<50%", "50 - 65%", "65 - 85%", ">85%")
        bin_qtr3 <- c("Null", "<50%", "50 - 65%", "65 - 85%", ">85%")
        bin_qtr4 <- c("Null", "<75%", "75 - 90%", "90 - 110%", ">110%")

        bin_cols <- c("#D9CDC3", "#e06714", "#f7e10f", "#7abe41", "#008000")

        bins <- NULL

        # Check bin cols
        bins <- case_when(
            qtr == 1 ~ bin_qtr1,
            qtr == 2 ~ bin_qtr2,
            qtr == 3 ~ bin_qtr3,
            qtr == 4 ~ bin_qtr4
        )

        # Classify Achievement
        df %>%
            mutate(
                achiev_bin = case_when(
                    is.na(achievement) ~ bins[1],
                    achievement < 50 ~ bins[2],
                    achievement >= 50 & achievement < 65 ~ bins[3],
                    achievement >= 65 & achievement <= 85 ~ bins[4],
                    TRUE ~ bins[5]
                ),
                achiev_bin = factor(achiev_bin, levels = bins),
                achiev_col = case_when(
                    is.na(achievement) ~ bin_cols[1],
                    achievement < 50 ~ bin_cols[2],
                    achievement >= 50 & achievement < 65 ~ bin_cols[3],
                    achievement >= 65 & achievement <= 85 ~ bin_cols[4],
                    TRUE ~ bin_cols[5]
                ),
                achiev_col = factor(achiev_col, levels = bin_cols)
            )
    }


    #' Viz of Achievements
    #'
    #' @param df
    #' @param indicator
    #' @param agency
    #' @param label
    #' @param achiev_bottom
    #'
    viz_agency_achievements <- function(df,
                                        indicator = "HTS_TST",
                                        agency = NULL,
                                        label = NULL,
                                        achiev_bottom = FALSE) {

        # Filter by indicator & agency
        df <- df %>% filter(indicator == {{indicator}})

        if (!is.null(agency)) {
            df <- df %>% filter(fundingagency == toupper({{agency}}))
        }


        # Result bars
        p <- ggplot(data = df, aes(x=fiscal_year)) +
            geom_col(aes(y=cumulative, fill = fundingagency),
                position = position_dodge(), show.legend = F)


        # Labels
        if (!is.null(label) && label == "targets") {
            p <- p + geom_text(aes(y = targets, label = comma(targets)),
                size = 3, color = grey90k, angle = 0, hjust = .5, vjust = -.5)
        }
        else if (!is.null(label) && label == "cumulative") {
            p <- p + geom_text(aes(y = cumulative, label = comma(cumulative)),
                size = 3, color = grey90k, angle = 0, hjust = .5, vjust = -.5)
        }
        else if (!is.null(label) && label == "both") {
            p <- p +
                geom_text(aes(y = cumulative, label = comma(cumulative)),
                    size = 3, color = grey90k, angle = 0, hjust = .5, vjust = -.5) +
                geom_text(aes(y = targets, label = comma(targets)),
                    size = 3, color = grey90k, angle = 0, hjust = .5, vjust = -.5)
        }

        # Target lines
        p <- p +
            geom_point(aes(y = targets),
                shape = 95, size = 18, color = "white", show.legend = F) +
            geom_point(aes(y = targets, color = fundingagency),
                shape = 95, size = 15, show.legend = F)


        # Achievements (%) - at the bottom
        if (achiev_bottom == TRUE) {

            p <- p +
                geom_point(aes(y = 1, fill = fundingagency),
                           shape = 21, size = 10, color = grey10k, show.legend = F) +
                geom_text(aes(y = 1, label = paste0(round(achievement, 0), "%")),
                          size = 3, color = 'white')
        }
        # Achievement (%) - in relation to targets / cumulative results
        else {

            # get scale factor / trans
            s_factor <- get_dualaxis(df, option = "scale")
            trans <- get_dualaxis(df, option = "trans")

            # Set up dual axis
            p <- p +
            geom_point(aes(y = achievement * s_factor, fill = fundingagency),
                       shape = 21, size = 10, color = grey10k, show.legend = F) +
            geom_text(aes(y = achievement * s_factor, label = paste0(round(achievement, 0), "%")),
                      size = 3, color = 'white') +
            scale_y_continuous(
                labels = comma,
                name = "Results / Targets",
                sec.axis = sec_axis(trans = trans, name = "Achievement (%)")
            )
        }

        # Color schemes + titles
        p <- p +
            scale_fill_manual(values = c("USAID" = si_blue, "CDC" = si_lblue, "DOD" = grey50k)) +
            scale_color_manual(values = c("USAID" = si_blue, "CDC" = si_lblue, "DOD" = grey50k)) +
            facet_grid(~fundingagency, switch = "x") +
            labs(
                title = paste0("DRC - ", {{indicator}}, " Achievements"),
                subtitle = "Achievement Trend by Agency - Think lines represent targets",
                caption = graph_note,
                x = "", y = ""
            )


        # Axis
        if (is.null(label)) {

            p <- p +
                si_style_ygrid() +
                theme(
                    legend.title = element_blank(),
                    axis.title.y.right = element_blank(),
                    axis.text.y.right = element_blank(),
                    strip.text = element_text(hjust = .5),
                    strip.placement = "outside"
                )
        }
        else {

            p <- p +
                si_style_nolines() +
                theme(
                    legend.title = element_blank(),
                    axis.title = element_blank(),
                    axis.text = element_blank(),
                    strip.text = element_text(hjust = .5),
                    strip.placement = "outside"
                )
        }

        return(p)
    }

    # Test
    viz_agency_achievements(df_cc_ag)

    viz_agency_achievements(df_cc_ag, label = "targets")
    viz_agency_achievements(df_cc_ag, label = "cumulative")
    viz_agency_achievements(df_cc_ag, label = "both")

    viz_agency_achievements(df_cc_ag, agency = "USAID")
    viz_agency_achievements(df_cc_ag, agency = "CDC")
    viz_agency_achievements(df_cc_ag, agency = "DOD")

    viz_agency_achievements(df_cc_ag, achiev_bottom = TRUE)

    viz_agency_achievements(df_cc_ag, agency = "DOD", achiev_bottom = TRUE)


# Data --------------------------------------

    ## MER - OU by IM

    df_mer_ou <- read_msd(file = here(dir_merdata, file_ou))

    df_mer_ou <- df_mer_ou %>%
        filter(operatingunit == curr_ou) %>%
        mutate(
            fundingagency = str_remove(fundingagency, "HHS/"),
            fundingagency = factor(fundingagency, levels = c("USAID", "CDC", "DOD"))
        )

    df_mer_ou %>% glimpse()

    df_mer_ou %>% distinct(operatingunit, fiscal_year)

    ## MER - Clinical Cascade Indicators

    df_cc_agency <- df_mer_ou %>%
        filter(
            fundingagency != "Dedup",
            indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW"),
            standardizeddisaggregate %in% c("Total Numerator")
        ) %>%
        select(fiscal_year, fundingagency, indicator, targets:cumulative) %>%
        group_by(fiscal_year, fundingagency, indicator) %>%
        summarise_at(vars(targets,cumulative), sum, na.rm = TRUE) %>%
        ungroup() %>%
        mutate(achievement = ifelse(!is.na(cumulative) & !is.na(targets), cumulative / targets * 100, NA))


    df_cc_agency %>% View()


# VIZ -------------------------------------

    df_cc_agency %>%
        distinct(indicator) %>%
        pull() %>%
        map(.x, .f = ~viz_agency_achievements(df = df_cc_agency, indicator = .x))

