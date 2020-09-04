## PROJECT:  SI Support for DRC
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Process Data submitted by IPs
## Date:     2020-08-04
## Updated:  2020-08-13

# LIBRARIES -------------------------------

library(tidyverse)
library(readxl)
library(vroom)
library(janitor)
library(arrow)

# CONFIGS --------------------------------

source("./Scripts/00_Config.R")

# GLABALS --------------------------------

dir_mreports <- "Data/Monthly Report"
dir_mr_fromips <- "Data/Monthly Report/From IPs"
dir_mr_creports <- "Data/Monthly Report/Compiled Reports"
dir_mr_datasets <- "Data/Monthly Report/Datasets"

# WORKFLOW ---------------------------------

    ### 0: Send templates to IPs for their monthly reports [Blank Templates]
    ### 1: Read raw data (xlsx) from IPs submissions [From IPs]
    ### 2: Wrangle data - from wide to long (csv) for each IP [Datasets]
    ### 3: Compile all datasets (csv) into one file (xlsx)


# FUNCTIONS

    #' Read submissions
    #'
    #' @param dta_folder Data folder
    #' @param rep_period Reporting Period
    #' @return A named list of data from mech_name / sheet_name as data frames
    #' @export
    #' @examples
    #'
    #' read_submissions("./Data/Monthly Report/From IPs")
    #'
    read_submissions <- function(dta_folder, rep_period = NULL) {

        ## Set reporting period
        if (is.null(rep_period)) {
            rep_period <- format(Sys.Date(), "%Y%m")
        }

        ## Extract file names
        files_submitted <- list.files(
            path = here(dta_folder, rep_period),
            pattern = ".xlsx",
            full.names = TRUE
        )

        ## Extract Mech names
        mechs_submitted <- files_submitted %>%
            basename() %>%
            str_remove("Monthly Reporting - |Template for Monthly Reporting - ") %>%
            str_remove(" - \\d{6}.xlsx$") %>%
            str_replace_all("-", "_")

        mechs_submitted

        ## Read all sheets within all files
        dfs_submitted <- files_submitted %>%
            set_names(nm = mechs_submitted) %>%
            map2(mechs_submitted, function(file_sub, mech_sub){

                print(paste("Mech name: ", mech_sub))

                # Extract sheet names
                shts <- file_sub %>%
                    excel_sheets()

                print("Sheets name:")
                print(shts)

                # LINKAGES has only 3 sheets
                if ( str_detect(toupper(file_sub), "LINKAGES") ) {
                    shts <- shts[1:3]
                }
                # The other 3 IMs have 10 sheets
                else {
                    shts <- shts[1:10]
                }

                # Read the content of each sheets
                dfs <- shts %>%
                    set_names(nm = str_replace_all(., " ", "_")) %>%
                    map(read_excel, path = file_sub, skip = 1)

                return(dfs)
            })

        return(dfs_submitted)
    }


    #' Process submissions
    #'
    #' @param tab_df Data from individual sheet as data frame
    #' @param tab_name Name of the sheet
    #' @return wrangled data as a data frame
    #' @export
    #' @examples
    #'
    #' process_submissions(tab_df = df, tab_name = "HTS_TST_FAC")
    #'
    process_submissions <- function(tab_df, tab_name) {

        print(tab_name)

        ## Flag level
        rep_lvl <- ifelse(
            str_detect(tolower(tab_name), "^hts_") & str_detect(tolower(tab_name), "_com$|_mobile_mod"),
            "Community",
            "Facility"
        )

        ## Remove total rows (1st rows)
        df <- tab_df %>%
            clean_names() %>%
            filter(!is.na(site_uid) & !str_detect(tolower(site_uid), "^totaux"))

        ## Diff. DSM has different exclusions: rm last 4
        if ( str_detect(tolower(tab_name), "^diff") ) {
            df <- df %>% select(1:(length(names(.)) - 4))
        }
        else {
            df <- df %>% select(!starts_with("total"))
        }

        ## Reshape from wide to long & transform
        df <- df %>%
            gather(key = "disaggregates", value = "value", -c(site_uid:high_volume)) %>%
            mutate(
                indicator = tab_name,
                indicator2 = indicator,
                reporting_level = rep_lvl,
                dlength = NA,
                result = NA,
                sex = NA,
                age = NA
            )

        # Unpack HTS_TST Disaggs
        if ( str_detect(tolower(tab_name), "^hts_") ) {

            df <- df %>%
                rowwise() %>%
                mutate(
                    dlength = length(unlist(str_split(disaggregates, "_"))),
                    result = str_to_title(last(unlist(str_split(disaggregates, "_")))),
                    sex = nth(unlist(str_split(disaggregates, "_")), n=dlength-1),
                    sex = str_to_title(str_remove(sex, "s$")),
                    age = ifelse(
                        dlength - 2 > 1,
                        paste(unlist(str_split(disaggregates, "_"))[1], unlist(str_split(disaggregates, "_"))[2], sep = "-"),
                        unlist(str_split(disaggregates, "_"))[1]
                    ),
                    age = ifelse(age == "x1", "<01", ifelse(age == "x50", "50+", str_remove(age, "x"))),
                    age = ifelse(age == "1-4", "01-04", ifelse(age == "5-9", "05-09", age)),
                    age = ifelse(age == "unknown-age", "Unknown Age", age)
                ) %>%
                ungroup() %>%
                mutate(
                    indicator2 = ifelse(
                        str_detect(tolower(indicator), "^hts_tst_a|^hts_tst_f|^hts_tst_c") & tolower(result) == "positive",
                        "HTS_TST_POS (N. DSD). HTS received results",
                        ifelse(
                            str_detect(tolower(indicator), "^hts_tst_a|^hts_tst_f|^hts_tst_c") & tolower(result) == "negative",
                            "HTS_TST_NEG (N. DSD). HTS received results",
                            ifelse(
                                str_detect(tolower(indicator), "^hts_tst_m") & tolower(result) == "positive",
                                "HTS_TST_POS (N. DSD. MbMod). HTS received results",
                                ifelse(
                                    str_detect(tolower(indicator), "^hts_tst_m") & tolower(result) == "negative",
                                    "HTS_TST_NEG (N. DSD. MbMod). HTS received results",
                                    ifelse(
                                        str_detect(tolower(indicator), "^hts_index") & tolower(result) == "positive",
                                        "HTS_INDEX_POS (N. DSD). HTS received results",
                                        ifelse(
                                            str_detect(tolower(indicator), "^hts_index") & tolower(result) == "negative",
                                            "HTS_INDEX_NEG (N. DSD). HTS received results",
                                            indicator2
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
        }
        ## TX
        else if ( str_detect(tolower(tab_name), "^tx_") ) {

            df <- df %>%
                rowwise() %>%
                mutate(
                    dlength = length(unlist(str_split(disaggregates, "_"))),
                    result = "Positive",
                    sex = last(unlist(str_split(disaggregates, "_"))),
                    sex = str_to_title(str_remove(sex, "s$")),
                    age = ifelse(
                        dlength - 1 > 1,
                        paste(unlist(str_split(disaggregates, "_"))[1], unlist(str_split(disaggregates, "_"))[2], sep = "-"),
                        unlist(str_split(disaggregates, "_"))[1]
                    ),
                    age = ifelse(age == "x1", "<01", ifelse(age == "x50", "50+", str_remove(age, "x"))),
                    age = ifelse(age == "1-4", "01-04", ifelse(age == "5-9", "05-09", age)),
                    age = ifelse(age == "unknown-age", "Unknown Age", age)
                ) %>%
                ungroup() %>%
                mutate(
                    indicator2 = ifelse(
                        str_detect(tolower(indicator), "^tx_curr"),
                        "TX_CURR (N. DSD. Age/Sex/HIVStatus). Receiving ART",
                        ifelse(
                            str_detect(tolower(indicator), "^tx_new"),
                            "TX_NEW (N. DSD. Age/Sex/HIVStatus). New on ART",
                            indicator2
                        )
                    )
                )
        }
        ## TB
        else if ( str_detect(tolower(tab_name), "^tb_prev_") ) {

            df <- df %>%
                mutate(
                    result = NA,
                    sex = ifelse(tolower(indicator) == "tb_prev_num" & str_detect(disaggregates, "_female"), "Female",
                                 ifelse(tolower(indicator) == "tb_prev_num" & str_detect(disaggregates, "_male"), "Male",
                                        sex)),
                    age = ifelse(tolower(indicator) == "tb_prev_num" & str_detect(disaggregates, "15_female$|_male$"), "<15",
                                 ifelse(tolower(indicator) == "tb_prev_num" & str_detect(disaggregates, "15_female_2$|_male_2$"), "15+",
                                        ifelse(tolower(indicator) == "tb_prev_num" & str_detect(disaggregates, "_unknown_age_f|_unknown_age_m"), "Unknown Age",
                                               age))),
                    indicator2 = ifelse(
                        tolower(indicator) == "tb_prev_num" & str_detect(disaggregates, "^ipt_newly_enrolled_on_art_"),
                        "IPT Newly enrolled on ART",
                        ifelse(
                            tolower(indicator) == "tb_prev_num" & str_detect(disaggregates, "^ipt_previously_enrolled_on_art_"),
                            "IPT Previously enrolled on ART",
                            ifelse(
                                tolower(indicator) == "tb_prev_num" & str_detect(disaggregates, "^alternative_tpt_regiment_newly_enrolled_on_art_"),
                                "Alternative TPT Regiment Newly enrolled on ART",
                                ifelse(
                                    tolower(indicator) == "tb_prev_num" & str_detect(disaggregates, "^alternative_tpt_regiment_previously_enrolled_on_art_"),
                                    "Alternative TPT Regiment Previously enrolled on ART",
                                    indicator2
                                )
                            )
                        )

                    )
                )
        }
        ## Diff. DSM
        else if (str_detect(tolower(tab_name), "^diff")) {

            df <- df %>%
                mutate(
                    indicator2 = case_when(
                        indicator == "Diff._Delivery_Service_Models" & disaggregates == "total_enrolled_in_podi" ~ "Enrolled in PODI",
                        indicator == "Diff._Delivery_Service_Models" & disaggregates == "total_enrolled_in_fast_track" ~ "Enrolled in Fast Track",
                        indicator == "Diff._Delivery_Service_Models" & disaggregates == "total_enrolled_in_com_art" ~ "Enrolled in Com ART",
                        indicator == "Diff._Delivery_Service_Models" & disaggregates == "total_enrolled_in_adherence_club" ~ "Enrolled in Adherence Club",
                        indicator == "Diff._Delivery_Service_Models" & disaggregates == "new_enrolled_in_podi" ~ "New Enrolled in PODI",
                        indicator == "Diff._Delivery_Service_Models" & disaggregates == "new_enrolled_in_fast_track" ~ "New Enrolled in Fast Track",
                        indicator == "Diff._Delivery_Service_Models" & disaggregates == "new_enrolled_in_com_art" ~ "ew Enrolled in Com ART",
                        indicator == "Diff._Delivery_Service_Models" & disaggregates == "new_enrolled_in_adherence_club" ~ "New Enrolled in Adherence Club",
                        indicator == "Diff._Delivery_Service_Models" & disaggregates == "number_non_elligible_yet" ~ "Non-Elligible Yet",
                        indicator == "Diff._Delivery_Service_Models" & disaggregates == "number_non_stable_patients" ~ "Non Stable Patients",
                        indicator == "Diff._Delivery_Service_Models" & disaggregates == "number_stable_patients_who_refused_to_adhere_to_a_model" ~ "Stable Patients who refused to adhere to a model",
                        TRUE ~ disaggregates
                    )
                )
        }
        ## VL
        else if (str_detect(tolower(tab_name), "^cv")) {

            df <- df %>%
                mutate(
                    indicator2 = case_when(
                        indicator == "CV" & disaggregates == "elligibles_attendus_pour_la_cv" ~ "Expected Elligible for the VL",
                        indicator == "CV" & disaggregates == "prelevement_effectues" ~ "Collected Specimens",
                        indicator == "CV" & disaggregates == "resultats_retires" ~ "Collected Results",
                        indicator == "CV" & disaggregates == "cv_supprimee" ~ "VL Suppressed",
                        TRUE ~ disaggregates
                    )
                )
        }
        ## Potential errors
        else {
            print(paste0("Unknown tab: ", tab_name))
        }

        ## Cleanup and reorder columns
        df <- df %>%
            mutate(indicator = indicator2) %>%
            select(-c(indicator2, dlength, disaggregates)) %>%
            relocate(indicator, age, sex, result, reporting_level, value, .after = high_volume)

        return(df)
    }


    #' Calculate TX_NET_NEW
    #'
    #' @param df Datasets from all IPs as data frame
    #' @param dta_folder Directory of Processed data folder
    #' @return Augmented dataset as a data frame
    #' @export
    #' @examples
    #'
    #' generate_net_new(df = df_proc, dta_folder = "./Data")
    #'
    calculate_net_new <- function(df, dta_folder = "./Data/Monthly Report/Datasets") {

        # Get current period
        curr_pd <- df_proc %>%
            filter(!is.na(period)) %>%
            distinct(period) %>%
            pull()

        print(curr_pd)

        # Identify previous period
        prev_pd <- as.character(as.integer(curr_pd) - 1)

        print(prev_pd)

        # Locate processed files from previous period
        files_processed <- list.files(
            path = here(dta_folder, prev_pd),
            pattern = "^DRC_Dataset \\d{6} - .*.csv$",
            full.names = TRUE
        )

        # Read only individual files
        files_processed <- files_processed[files_processed %>% str_detect(".ALL.csv$", negate = TRUE)]

        # Check length
        if (length(files_processed) != 4) {

            print("ERROR - looks like the number of files != 4")

            print(files_processed)

            return(NULL)
        }

        # Label for new indicator: TX_NET_NEW
        lbl_tx_curr <- "TX_CURR (N. DSD. Age/Sex/HIVStatus). Receiving ART"
        lbl_tx_net_new <- "TX_NET_NEW (N. DSD. Age/Sex/HIVStatus). New on ART"

        # Extract previous TX_CURR data
        df_tx_curr <- files_processed %>%
            map_dfr(vroom) %>%
            clean_names() %>%
            select(-period) %>%
            filter(str_detect(indicator, "^TX_CURR")) %>%
            mutate(high_volume = as.character(high_volume)) %>%
            mutate(indicator = "tx_curr_prev") %>%              # TX_CURR for previous period
            spread(indicator, value)

        # Calculate TX_NET_NEW based on current and previous TX_CURR's values
        df <- df %>%
            filter(str_detect(indicator, "^TX_CURR")) %>%
            mutate(indicator = "tx_curr_curr") %>%              # TX_CURR for current period
            spread(indicator, value) %>%
            left_join(df_tx_curr, by = c("site_uid", "implementing_mechanism", "province", "health_zone",
                                         "site", "high_volume", "age", "sex", "result", "reporting_level")
            ) %>%
            mutate(tx_net_new = ifelse(!is.na(tx_curr_curr) & !is.na(tx_curr_prev),
                                       tx_curr_curr - tx_curr_prev, NA)) %>%
            select(-c(tx_curr_curr, tx_curr_prev)) %>%
            gather(indicator, value, tx_net_new) %>%
            mutate(indicator = lbl_tx_net_new) %>%
            relocate(indicator, .after = high_volume) %>%
            bind_rows(df)

        # return expanded data frame
        return(df)
    }


    #' Export processed files
    #'
    #' @param df_proc
    #' @param output_folder
    #' @return void
    #' @export
    #' @examples
    #'
    #' export_partners_report(df_proc, output_folder = "./Data/Monthly Report/Datasets")
    #'
    export_partners_report <- function(df_proc, output_folder, rep_period = NULL) {

        ## Set reporting period
        if (is.null(rep_period)) {
            rep_period <- format(Sys.Date(), "%Y%m")
        }

        print(rep_period)

        ## Create directory if it does not exist
        ddir <- paste0(output_folder, "/", rep_period)

        if ( !dir.exists(ddir) ) {
            dir.create(ddir)
        }

        ## Filter out NA is any
        df_proc <- df_proc %>%
            filter(!is.na(implementing_mechanism))

        ## Export entire dataset
        print("Exporting entire dataset")

        dname <- paste0(output_folder, "/", rep_period, "/", "DRC_Dataset ", rep_period, " - ALL - ", format(Sys.Date(), "%Y%m%d"), ".csv")

        print(dname)

        df_proc %>%
            filter(!is.na(implementing_mechanism)) %>%
            write_csv(path = dname, na = "")

        ## Export dateset by IM
        print("Exporting dataset by im:")

        ## Unique IMs
        ims <-  df_proc %>%
            distinct(implementing_mechanism) %>%
            pull()

        ## Export
        ims %>%
            map(function(im){

                print(im)

                fname <- paste0(output_folder, "/", rep_period, "/", "DRC_Dataset ", rep_period, " - ", im, " - ", format(Sys.Date(), "%Y%m%d"), ".csv")

                print(fname)

                df_proc %>%
                    filter(!is.na(implementing_mechanism) & implementing_mechanism == im) %>%
                    write_csv(path = fname, na = "")

                return(im)
            })
    }



# TEST ------------------------------------

    ## Test - read data from submissions
    ## Note: dfs > file[IM] > sheet[INDICATOR]
    dfs_subs <- read_submissions(dta_folder = "./Data/Monthly Report/From IPs",
                                 rep_period = "202006")

    dfs_subs$IHAP_HK$HTS_TST_FAC %>% glimpse()

    ## Test - process data
    ## 1 tab/file at the time
    df_proc <- dfs_subs %>%
        map_dfr(function(ip_data) {
            ip_data %>% map2_dfr(names(.), process_submissions)
        })

    df_proc %>% glimpse()

    df_proc %>%
        filter(!is.na(period)) %>%
        distinct(period) %>%
        pull()

    df_proc %>%
        distinct(indicator) %>%
        pull()

    df_proc %>%
        distinct(implementing_mechanism) %>%
        pull()

    df_proc %>%
        filter(is.na(implementing_mechanism))

    ## Test - Generate Net NEW
    ## Read TX_CURR from previous files and calcuate TX_NET_NEW
    df_proc <- df_proc %>%
        calculate_net_new(dta_folder = "./Data/Monthly Report/Datasets")

    df_proc %>% glimpse()


    ## Test - Export ip report data
    export_partners_report(df_proc,
                           output_folder = "./Data/Monthly Report/Datasets",
                           rep_period = "202006")

# DATA -----------------------------------

    # Compiled data
    files_compiled <- list.files(
        path = dir_mr_creports,
        pattern = "^Import File - \\d{6} - \\d{8}.xlsx$",
        full.names = TRUE,
        recursive = TRUE
    )

    files_compiled

    sheets_compiled <- files_compiled %>% excel_sheets()

    sheets_compiled
    #
    # [1] "HTS_TST_FAC"
    # [2] "HTS_TST_COM"
    # [3] "HTS_INDEX_FAC"
    # [4] "HTS_INDEX_COM"
    # [5] "HTS_TST_Mb_Mod"
    # [6] "TX_NEW"
    # [7] "TX_CURR"
    # [8] "TB_PREV"
    # [9] "Diff. Delivery Service Models"
    # [10] "VL"
    # [11] "HTS_TST_FAC_1"
    # [12] "HTS_TST_COM_1"
    # [13] "HTS_INDEX_FAC_1"
    # [14] "HTS_INDEX_COM_1"
    # [15] "HTS_TST_Mb_Mod_1"
    # [16] "TX_NEW_1"
    # [17] "TX_CURR_1"
    # [18] "TX_NET_NEW Pivot"
    # [19] "TX_NET_NEW"
    # [20] "TX_NET_NEW_1"
    # [21] "TB_PREV_1"
    # [22] "DSD_1"
    # [23] "VL_1"
    # [24] "Dataset"

    ## read all tabs from compiled file
    dfs_compiled <- files_compiled %>%
        excel_sheets() %>%
        set_names() %>%
        map(read_excel, path = files_compiled)

    dfs_compiled

    dfs_compiled$Dataset %>% glimpse()

    dfs_compiled$Dataset %>%
        distinct(indicator) %>%
        pull()

    ## Processed datasets
    data_processed <- list.files(
            path = here(dir_mr_datasets, "202005"),
            pattern = "^DRC_Dataset \\d{6} - .*.csv$",
            full.names = TRUE
        ) %>%
        first() %>%
        map_dfr(vroom)

    data_processed %>% glimpse()

    View(data_processed)

    ## Columns
    data_processed %>% colnames()
    # [1] "site_uid"               "period"
    # [3] "implementing_mechanism" "province"
    # [5] "health_zone"            "site"
    # [7] "high_volume"            "indicator"
    # [9] "age"                    "sex"
    # [11] "result"                 "reporting_level"
    # [13] "Value"

    ## Age Groups
    data_processed %>% distinct(age) %>% pull()
    # [1] "<01"         "01-04"       "05-09"       "10-14"
    # [5] "15-19"       "20-24"       "25-29"       "30-34"
    # [9] "35-39"       "40-44"       "45-49"       "50+"
    # [13] "Unknown Age" "<15"         "15+"         NA

    ## Age Groups
    data_processed %>% distinct(sex) %>% pull()
    #[1] "Female" "Male"   NA

    ## Results
    data_processed %>% distinct(result) %>% pull()
    #[1] "Positive" "Negative" NA

    ## Reporting levels
    data_processed %>% distinct(reporting_level) %>% pull()
    #[1] "Facility"  "Community"

    ## Indicators
    data_processed %>%
        distinct(indicator) %>%
        pull()

    ## HTS [Age/Sex/HIVStatus]
    # [1] "HTS_TST_POS (N. DSD). HTS received results"
    # [2] "HTS_TST_NEG (N. DSD). HTS received results"
    # [3] "HTS_INDEX_POS (N. DSD). HTS received results"
    # [4] "HTS_INDEX_NEG (N. DSD). HTS received results"
    # [5] "HTS_TST_POS (N. DSD. MbMod). HTS received results"
    # [6] "HTS_TST_NEG (N. DSD. MbMod). HTS received results"
    ## TX [Age/Sex]
    # [7] "TX_NEW (N. DSD. Age/Sex/HIVStatus). New on ART"
    # [8] "TX_CURR (N. DSD. Age/Sex/HIVStatus). Receiving ART"
    # [9] "TX_NET_NEW (N. DSD. Age/Sex/HIVStatus). New on ART"
    ## TB [Mod/Age/Sex]
    # [10] "IPT Newly enrolled on ART"
    # [11] "IPT Previously enrolled on ART"
    ## Diff. DSM
    # [12] "Enrolled in PODI"
    # [13] "Enrolled in Fast Track"
    # [14] "Enrolled in Com ART"
    # [15] "New Enrolled in PODI"
    # [16] "New Enrolled in Fast Track"
    # [17] "New Enrolled in Com ART"
    ## CV
    # [18] "Expected Elligible for the VL"
    # [19] "Collected Specimens"
    # [20] "Collected Results"
    # [21] "VL Suppressed"
    ## Diff. DSM
    # [22] "Enrolled in Adherence Club"
    # [23] "New Enrolled in Adherence Club"
    # [24] "Non-Elligible Yet"
    # [25] "Non Stable Patients"
    # [26] "Stable Patients who refused to adhere to a model"
    ## TB [Mod/Age/Sex]
    # [27] "Alternative TPT Regiment Newly enrolled on ART"
    # [28] "Alternative TPT Regiment Previously enrolled on ART"

    ## Raw data
    files_submitted <- list.files(
            path = here(dir_mr_fromips, "202006"),
            pattern = ".xlsx",
            full.names = TRUE
        )

    files_submitted

    mechs_submitted <- files_submitted %>%
        basename() %>%
        str_remove("Monthly Reporting - |Template for Monthly Reporting - ") %>%
        str_remove(" - \\d{6}.xlsx$") %>%
        str_replace_all("-", "_")

    mechs_submitted

    ## Tabs in the files
    files_submitted %>%
        set_names(nm = mechs_submitted) %>%
        map(excel_sheets)

    ##$LINKAGES
    #
    # [1] "HTS_TST All Modalities"
    # [2] "HTS_INDEX_COM"
    # [3] "HTS_TST Mobile Mod"

    ##$IHAP_HK, IHAP_KIN & KHETH_IMPILO
    #
    # [1] "HTS_TST FAC"
    # [2] "HTS_TST COM"
    # [3] "HTS_INDEX_FAC"
    # [4] "HTS_INDEX_COM"
    # [5] "HTS_TST Mobile Mod"
    # [6] "TX_NEW"
    # [7] "TX_CURR"
    # [8] "TB_PREV Num"
    # [9] "Diff. Delivery Service Models"
    # [10] "CV"

    ## Stu
    dfs_submitted <- files_submitted %>%
        set_names(nm = mechs_submitted) %>%
        map(function(file_sub){

            # Extract list of sheet names
            shts <- file_sub %>%
                excel_sheets()

            # LINKAGES has only 3 sheets
            if ( str_detect(toupper(file_sub), "LINKAGES") ) {
                shts <- shts[1:3]
            }
            # The other 3 IMs have 10 sheets
            else {
                shts <- shts[1:10]
            }

            # Read the content of each sheets
            dfs <- shts %>%
                set_names(nm = str_replace_all(., " ", "_")) %>%
                map(read_excel, path = file_sub)

            return(dfs)
        })

    dfs_submitted

    dfs_submitted$LINKAGES
    dfs_submitted$IHAP_HK
    dfs_submitted$IHAP_KIN$TB_PREV_Num %>% glimpse()




