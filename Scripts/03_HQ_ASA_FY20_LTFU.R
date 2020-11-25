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
library(scales)

# SOURCE ----------------------------------

source("./Scripts/00_Config.R")
source("../lastmile/Scripts/00_Utilities.R")
source("../lastmile/Scripts/00_Geo_Utilities.R")
source("../lastmile/Scripts/00_ML_Utilities.R")

source("./Scripts/03_HQ_AA_FY20_HealthZones.R")

# GLOBAL ------------------------------------

country <- "Democratic Republic of the Congo"

agency <- "USAID"

agencies <- c("USAID", "HHS/CDC")

curr_pd <- 4

curr_fy <- 2020

curr_date <- format(Sys.Date(), "%Y%m%d")

ml_caption <- paste0(
    "OHA/SIEI - DRC LTFU, Data Source: FY20Q4i MSD, Produced on ",
    curr_date)


# FUNCTIONS ----------------------------------



# DATA --------------------------------------

    # DRC MSD Data
    df_msd <- list.files(
            path = dir_merdata,
            pattern = "MER_S.*_PSNU_IM_.*_20201113_v1_1_.*Congo.zip$",
            full.names = TRUE
        ) %>%
        sort() %>%
        last() %>%
        read_msd()

    df_msd %>% glimpse()

    # Geodata
    spdf_pepfar <- build_spdf(
        dir_geo = paste0(dir_geodata, "/VcPepfarPolygons_2020.07.24"),
        df_psnu = df_msd)

    # Raster
    terr <- get_raster()

    ## LTFU
    df_ltfu <- df_msd %>%
        extract_tx_ltfu(rep_agency = agency, snu_prio = "psnu")

    df_ltfu <- df_ltfu %>%
        mutate(
            snu1 = factor(
                snu1,
                levels = c("Haut Katanga", "Lualaba", "Kinshasa")
            )
        )

    df_ltfu %>% glimpse()

    # SPDF LTFU
    spdf_ltfu <- spdf_pepfar %>%
        left_join(df_ltfu, by = c("uid" = "psnuuid")) %>%
        rename_at(vars(ends_with(".y")), str_remove, ".y") %>%
        filter(!is.na(psnu))


# VIZ -------------------------------------

    # Plots ----------

        # Country
        plot_ltfu <- plot_tx_ltfu(df_msd = df_ltfu) +
            facet_grid(snu1 ~ ., scales = "free",
                       space = "free", switch = "both") +
            theme(strip.placement = "outside",
                  strip.text = element_text(hjust = .5))

        plot_ltfu

        ggsave(
            here::here("Graphics", "FY20Q4_DRC_Plot_TX_LTFU.png"),
            plot = last_plot(),
            scale = 1.2,
            dpi = 400,
            width = 10,
            height = 7,
            units = "in"
        )

        # SE Only
        plot_ltfu_se <- plot_tx_ltfu(df_msd = df_ltfu %>% filter(snu1 != "Kinshasa")) +
            facet_grid(snu1 ~ ., scales = "free",
                       space = "free", switch = "both") +
            theme(strip.placement = "outside",
                  strip.text = element_text(hjust = .5))

        plot_ltfu_se

        ggsave(
            here::here("Graphics", "FY20Q4_DRC_Plot_TX_LTFU_for_HK_and_Lualaba.png"),
            plot = last_plot(),
            scale = 1.2,
            dpi = 400,
            width = 10,
            height = 7,
            units = "in"
        )

        # Kinshasa Only
        plot_ltfu_kin <- plot_tx_ltfu(df_msd = df_ltfu %>% filter(snu1 == "Kinshasa")) +
            facet_grid(snu1 ~ ., scales = "free",
                       space = "free", switch = "both") +
            theme(strip.placement = "outside",
                  strip.text = element_text(hjust = .5))

        plot_ltfu_kin

        ggsave(
            here::here("Graphics", "FY20Q4_DRC_Plot_TX_LTFU_for_Kinshasa.png"),
            plot = last_plot(),
            scale = 1.2,
            dpi = 400,
            width = 10,
            height = 7,
            units = "in"
        )

    # Maps -------------

        # SE Only
        map_ltfu_se <- se_base +
            geom_sf(data = spdf_ltfu %>%
                        filter(snu1 != "Kinshasa", !is.na(otherdisagg)),
                    aes(fill = prct),
                    lwd = .5,
                    color = grey10k,
                    alpha = .7) +
            geom_sf(data = adm1_se, fill = NA, size = .5) +
            geom_sf_text(data = adm1_se, aes(label = province),
                         size = 3, color = grey70k) +
            scale_fill_viridis_c(direction = -1, na.value = NA, limits = c(1, 100)) +
            facet_wrap(~otherdisagg, ncol = 2)

        map_ltfu_se

        ggsave(
            here::here("Graphics", "FY20Q4_DRC_Map_TX_LTFU_for_HK_and_Lualaba.png"),
            plot = last_plot(),
            scale = 1.2,
            dpi = 400,
            width = 10,
            height = 7,
            units = "in"
        )


        # SE Only
        map_ltfu_kin <- kin_base +
            geom_sf(data = hzones_kin, fill = NA,
                    linetype = "dotted", color = grey40k, size = .2) +
            geom_sf(data = spdf_ltfu %>%
                        filter(snu1 == "Kinshasa", !is.na(otherdisagg)),
                    aes(fill = prct),
                    lwd = .5,
                    color = grey10k,
                    alpha = .7) +
            geom_sf_text(data = adm1_kin, aes(label = province),
                         size = 3, color = grey70k) +
            scale_fill_viridis_c(direction = -1, na.value = NA, limits = c(1, 100)) +
            facet_wrap(~otherdisagg, ncol = 2)

        map_ltfu_kin

        ggsave(
            here::here("Graphics", "FY20Q4_DRC_Map_TX_LTFU_for_Kinshasa.png"),
            plot = last_plot(),
            scale = 1.2,
            dpi = 400,
            width = 10,
            height = 7,
            units = "in"
        )


    # Graphs ------------

        # SE Only
        graph_ltfu_se <- (map_ltfu_se + plot_ltfu_se) +
            plot_annotation(
                title = "DRC - ART Patients with no clinical contact since their last expected contacts",
                subtitle = "Data is aggregated by PSNU and filter for USAID Only",
                caption = ml_caption
            ) +
            theme(
                text = element_text(family = "Source Sans Pro")
            )

        graph_ltfu_se

        ggsave(
            here::here("Graphics", "FY20Q4_DRC_Graph_TX_LTFU_for_HK_and_Lualaba.png"),
            plot = last_plot(),
            scale = 1.2,
            dpi = 400,
            width = 10,
            height = 7,
            units = "in"
        )


        # Kinshasa Only
        graph_ltfu_kin <- (map_ltfu_kin + plot_ltfu_kin) +
            plot_annotation(
                title = "DRC - ART Patients with no clinical contact since their last expected contacts",
                subtitle = "Data is aggregated by PSNU and filter for USAID Only",
                caption = ml_caption
            ) +
            theme(
                text = element_text(family = "Source Sans Pro")
            )

        graph_ltfu_kin

        ggsave(
            here::here("Graphics", "FY20Q4_DRC_Graph_TX_LTFU_for_Kinshasa.png"),
            plot = last_plot(),
            scale = 1.2,
            dpi = 400,
            width = 10,
            height = 7,
            units = "in"
        )



