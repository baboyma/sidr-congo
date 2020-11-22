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
library(raster)
library(sf)
library(sp)
library(gisr)
library(glitr)
library(here)
library(ICPIutilities)
library(patchwork) # use of inset_element
library(extrafont)
library(glue)

# SOURCE ----------------------------------

source("./Scripts/00_Config.R")

# GLOBAL ------------------------------------

pal <- RColorBrewer::brewer.pal(5, "Spectral")[2:5]

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
    vroom()

df_psnu_sum %>% glimpse()
View(df_psnu_sum)


# VIZ ----------------------------------------------------

## Country Table

df_cntry_sum %>% gt()

df_cntry_sum %>%
    filter(fundingagency == "USAID") %>%
    dplyr::select(-fundingagency) %>%
    mutate(
        fy2019achive = case_when(
            !is.na(fy2019cumulative) &
                !is.na(fy2019_targets) ~
                round(fy2019cumulative / fy2019_targets * 100, 2)
        ),
        fy2020achive = case_when(
            !is.na(fy2020cumulative) &
                !is.na(fy2020_targets) ~
                round(fy2020cumulative / fy2020_targets * 100, 2)
        )
    ) %>%
    gt(rowname_col = "indicator") %>%
    tab_header(
        title = "DRC - Agency Performance",
        subtitle = "FY29 & 20 USAID Only Summary"
    ) %>%
    tab_spanner(
        label = "2019",
        columns = starts_with("fy2019")
    ) %>%
    tab_spanner(
        label = "2020",
        columns = starts_with("fy2020")
    ) %>%
    cols_label(
        fy2019q1 = "Q1",
        fy2019q2 = "Q2",
        fy2019q3 = "Q3",
        fy2019q4 = "Q4",
        fy2019cumulative = "Cumulative",
        fy2019_targets = "Targets",
        fy2019achive = "Achievement",
        fy2020q1 = "Q1",
        fy2020q2 = "Q2",
        fy2020q3 = "Q3",
        fy2020q4 = "Q4",
        fy2020cumulative = "Cumulative",
        fy2020_targets = "Targets",
        fy2020achive = "Achievement"
    ) %>%
    fmt_number(
        everything(),
        decimals = 0
    ) %>%
    fmt_percent(
        ends_with("achieve"),
        decimals = 0
    ) %>%
    fmt_missing(
        everything(),
        missing_text = ""
    ) %>%
    tab_style(
        style = cell_borders(
            sides = "right",
            weight = px(1.5),
        ),
        locations = cells_body(
            columns = everything(),
            rows = everything()
        )
    ) %>%
    tab_source_note(
        source_note = "Data Source: FY20Q4i MSD"
    )



