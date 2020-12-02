## PROJECT:  SI Support for DRC
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  DRC/PEPFAR Health Zones
## Date:     2020-11-20

# LIBRARIES -------------------------------

library(tidyverse)
library(cowplot)  # Use ggdraw & draw_plot
library(gt)       # Pretty tables
library(vroom)
library(janitor)
library(glitr)
library(scales)
library(RColorBrewer)
library(here)
library(patchwork) # use of inset_element
library(extrafont)
library(glue)

# SOURCE ----------------------------------

source("./Scripts/00_Config.R")
source("../_setup/00_Setup.R")

# GLOBAL ------------------------------------

agency <- "USAID"

RColorBrewer::display.brewer.all()

pal <- brewer.pal(5, "Spectral")[2:5]

# DATA --------------------------------------------------

# Country MSD Summary
df_cntry_sum <- list.files(
        path = dir_dataout,
        pattern = "^Demo.* - Overview_tbl_q4_\\d{8}.csv$",
        full.names = TRUE
    ) %>%
    sort() %>%
    last() %>%
    vroom()

df_cntry_sum %>% glimpse()

View(df_cntry_sum)


# SNU1 MSD Summary
df_snu1_sum <- list.files(
        path = dir_dataout,
        pattern = "^Demo.* - Overview_snu1_tbl_q4_\\d{8}.csv$",
        full.names = TRUE
    ) %>%
    sort() %>%
    last() %>%
    vroom()

df_snu1_sum %>% glimpse()

View(df_snu1_sum)


# SNU1 MSD Summary
df_psnu_sum <- list.files(
        path = dir_dataout,
        pattern = "^Demo.* - Overview_psnu_tbl_q4_\\d{8}.csv$",
        full.names = TRUE
    ) %>%
    sort() %>%
    last() %>%
    vroom() %>%
    dplyr::select(-psnuuid)

df_psnu_sum %>% glimpse()
View(df_psnu_sum)


# VIZ ----------------------------------------------------

## Country Table -----------------

    ## Columns -------------

        #  Cols - All original
        cols <- df_cntry_sum %>%
            dplyr::select(-fundingagency) %>%
            names()

        # Cols - Reporting Periods
        cols_qtr <- df_cntry_sum %>%
            dplyr::select(matches("^fy\\d{4}q\\d{1}$")) %>%
            names()

        # Cols - Targets
        cols_trgt <- df_cntry_sum %>%
            dplyr::select(ends_with("targets")) %>%
            names()

        # Cols - Cumulative
        cols_cum <- df_cntry_sum %>%
            dplyr::select(ends_with("cumulative")) %>%
            names()

        # Cols - Achievements
        cols_ach <- NULL

        # Cols - Gap
        cols_gap <- NULL

    ## Rows --------------------------------------

        # Rowsnames
        rows <- df_cntry_sum %>%
            rownames() %>%
            as.integer()

        # rows - values
        rows_val <- df_cntry_sum %>%
            mutate(row_id = row_number()) %>%
            filter(fundingagency == agency,
                   str_detect(indicator, "[:upper:]")) %>%
            pull(row_id)

        # rows - calculated percentages
        rows_prct <- df_cntry_sum %>%
            mutate(row_id = row_number()) %>%
            filter(
                fundingagency == agency,
                !str_detect(indicator, "[:upper:]")
            ) %>%
            pull(row_id)



# ViZ original data
df_cntry_sum %>% gt()

# Summarise data
df_cntry_sum <- df_cntry_sum %>%
    filter(fundingagency == "USAID") %>%
    dplyr::select(-fundingagency) %>%
    mutate(
        fy2019achieve = case_when(
            str_detect(indicator, "[:upper:]") & !is.na(fy2019cumulative) &
                !is.na(fy2019_targets) ~
                round(fy2019cumulative / fy2019_targets * 100, 2)
        ),
        fy2020achieve = case_when(
            str_detect(indicator, "[:upper:]") & !is.na(fy2020cumulative) &
                !is.na(fy2020_targets) ~
                round(fy2020cumulative / fy2020_targets * 100, 2)
        ),
        fy2020gap = case_when(
            !is.na(fy2020cumulative) &
                !is.na(fy2020_targets) ~
                (fy2020_targets - fy2020cumulative)
        ),
        fy2020gap = case_when(
            !is.na(fy2020gap) & fy2020gap > 0 ~ fy2020gap
        )
    )

# Update cols list
cols_ach <- df_cntry_sum %>%
    dplyr::select(ends_with("achieve")) %>%
    names()

# Cols - Gap
cols_gap <- df_cntry_sum %>%
    dplyr::select(ends_with("gap")) %>%
    names()

# Rows - Gap
rows_gap <- df_cntry_sum %>%
    mutate(row_id = row_number()) %>%
    filter(!is.na(fy2020gap)) %>%
    pull(row_id)

# Rows - FY19 Achievement
rows_ach1_lt60 <- df_cntry_sum %>%
    mutate(row_id = row_number()) %>%
    filter(fy2019achieve < 60) %>%
    pull(row_id)

rows_ach1_6080 <- df_cntry_sum %>%
    mutate(row_id = row_number()) %>%
    filter(fy2019achieve > 60 & fy2019achieve < 80) %>%
    pull(row_id)

rows_ach1_8090 <- df_cntry_sum %>%
    mutate(row_id = row_number()) %>%
    filter(fy2019achieve > 80 & fy2019achieve < 90) %>%
    pull(row_id)

rows_ach1_gt90 <- df_cntry_sum %>%
    mutate(row_id = row_number()) %>%
    filter(fy2019achieve >= 90) %>%
    pull(row_id)

# Rows - FY20 Achievement
rows_ach2_lt60 <- df_cntry_sum %>%
    mutate(row_id = row_number()) %>%
    filter(fy2020achieve < 60) %>%
    pull(row_id)

rows_ach2_6080 <- df_cntry_sum %>%
    mutate(row_id = row_number()) %>%
    filter(fy2020achieve > 60 & fy2002achieve < 80) %>%
    pull(row_id)

rows_ach2_8090 <- df_cntry_sum %>%
    mutate(row_id = row_number()) %>%
    filter(fy2020achieve > 80 & fy2020achieve < 90) %>%
    pull(row_id)

rows_ach2_gt90 <- df_cntry_sum %>%
    mutate(row_id = row_number()) %>%
    filter(fy2020achieve >= 90) %>%
    pull(row_id)


# Beautifier table
df_cntry_sum %>%
    gt(rowname_col = "indicator") %>%
    tab_header(
        title = "DRC - PEPFAR Programs Performance Overview",
        subtitle = "FY19 & 20, USAID Only Performance Summary"
    ) %>%
    tab_spanner(
        label = "2019",
        columns = starts_with("fy2019")
    ) %>%
    tab_spanner(
        label = "2020",
        columns = starts_with("fy2020")
    ) %>%
    # Reformat Column namess
    cols_label(
        fy2019q1 = "Q1",
        fy2019q2 = "Q2",
        fy2019q3 = "Q3",
        fy2019q4 = "Q4",
        fy2019cumulative = "Cumulative",
        fy2019_targets = "Targets",
        fy2019achieve = "Achievement",
        fy2020q1 = "Q1",
        fy2020q2 = "Q2",
        fy2020q3 = "Q3",
        fy2020q4 = "Q4",
        fy2020cumulative = "Cumulative",
        fy2020_targets = "Targets",
        fy2020achieve = "Achievement",
        fy2020gap = "Gap"
    ) %>%
    # Append percentage signe
    text_transform(
        locations = list(
            cells_body(columns = cols_ach),
            cells_body(columns = cols_qtr, rows = rows_prct),
            cells_body(columns = c(cols_cum, cols_trgt), rows = rows_prct[2:3])
        ),
        fn = function(x) {
            case_when(
                is.na(x) | x != "" ~ paste0(x, "%"),
                TRUE ~ ""
            )
        }
    ) %>%
    # Format numeric/missing values
    fmt_number(everything(), decimals = 0) %>%
    #fmt_number(cols_ach, decimals = 2) %>%
    #fmt_percent(cols_ach, decimals = 1) %>%
    fmt_missing(everything(), missing_text = "") %>%
    # Borders
    tab_style(
        style = cell_borders(
            sides = c("top", "bottom"),
            weight = px(1),
            color = grey50k
        ),
        locations = cells_body(
            columns = everything(),
            rows = everything()
        )
    ) %>%
    # Bold and italic text
    tab_style(
        style = cell_text(
            weight = "bold",
            style = "italic"
        ),
        locations = cells_body(
            columns = c(cols_cum, cols_trgt, cols_gap),
            rows = everything()
        )
    ) %>%
    # Achievement lt60
    tab_style(
        style = cell_text(
            color = USAID_dkred,
            weight = "bold",
            style = "italic"
        ),
        locations = list(
            cells_body(
                columns = cols_ach %>% first(),
                rows = rows_ach1_lt60
            ),
            cells_body(
                columns = cols_ach %>% last(),
                rows = rows_ach2_lt60
            )
        )
    ) %>%
    # Achievement 60 - 80
    tab_style(
        style = cell_text(
            color = si_orange,
            weight = "bold",
            style = "italic"
        ),
        locations = list(
            cells_body(
                columns = cols_ach %>% first(),
                rows = rows_ach1_6080
            ),
            cells_body(
                columns = cols_ach %>% last(),
                rows = rows_ach2_6080
            )
        )
    ) %>%
    # Achievement 80 - 90
    tab_style(
        style = cell_text(
            color = si_lorange,
            weight = "bold",
            style = "italic"
        ),
        locations = list(
            cells_body(
                columns = cols_ach %>% first(),
                rows = rows_ach1_8090
            ),
            cells_body(
                columns = cols_ach %>% last(),
                rows = rows_ach2_8090
            )
        )
    ) %>%
    # Achievement > 90
    tab_style(
        style = cell_text(
            color = wapo_lgreen,
            weight = "bold",
            style = "italic"
        ),
        locations = list(
            cells_body(
                columns = cols_ach %>% first(),
                rows = rows_ach1_gt90
            ),
            cells_body(
                columns = cols_ach %>% last(),
                rows = rows_ach2_gt90
            )
        )
    ) %>%
    # Hightlight FY20 Gaps
    tab_style(
        style = cell_text(
            color = USAID_red,
            style = "italic"
        ),
        locations = cells_body(
            columns = cols_gap,
            rows = rows_gap
        )
    ) %>%
    # Hightlight everyother rows
    tab_style(
        style = cell_fill(
            color = USAID_ltblue,
            alpha = .2
        ),
        locations = cells_body(
            columns = everything(),
            rows = rows[c(TRUE, FALSE)] #rows_prct
        )
    ) %>%
    tab_source_note(
        source_note = paste0("Data Source: FY20Q4i MSD, ",
                             "Produced by OHA/SIEI/SI on ",
                             format(Sys.Date(), "%Y-%m-%d"))
    )



