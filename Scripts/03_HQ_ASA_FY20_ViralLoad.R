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
source("../lastmile/Scripts/00_Geo_Utilities.R")
source("../lastmile/Scripts/00_VL_Utilities.R")

# GLOBAL ------------------------------------

country <- "Democratic Republic of the Congo"

agency <- "USAID"

agencies <- c("USAID", "HHS/CDC")

curr_pd <- 4

curr_fy <- 2020

curr_date <- format(Sys.Date(), "%Y%m%d")

vl_caption <- paste0("OHA/SIEI - DRC ViralLoad, MSD - FY20Q4i, Produced on ",
                      curr_date)


# FUNCTIONS ----------------------------------

#' @title Generate plot title
#'
#' @param country country name
#' @param var df variable
#' @return plot caption
#' @examples
#'
get_title <-
    function(var, peds = FALSE,
             country = NULL) {

        title <- toupper({{country}})

        var_name <- toupper({{var}})

        var_label <- ""

        # Get label
        if (var_name == "VLS") {
            var_label <- "Viral Load Suppression"

        } else if (var_name == "VLC") {
            var_label <- "Viral Load Coverage"

        } else if (var_name == "VLNC") {
            var_label <- "Viral Load Not-Covered"

        } else {
            var_label <- "Viral Load"
        }

        # PEDS flag
        if (peds == TRUE) {
            var_label <- paste0(var_label, " (Under 15yo)")
        }

        # Title
        if (!is.null({{country}})) {
            title <- paste0(toupper({{country}}), " - ", var_label)

        } else {
            title <- var_label
        }

        return(title)
    }


#' @title Generate Caption for Graphic/Output
#'
#' @param country country name
#' @param var df variable
#' @return plot caption
#'
get_caption <-
    function(country,
             rep_pd = "FY20Q4i",
             var = NULL) {

        caption <- paste0("OHA/SIEI - Data Source: ",
                          {{rep_pd}},
                          " MSD, Missing data in grey,
                      VLC = TX_PVLS / TX_CURR (2 periods prior), Not Covered = 1 - VLC\n",
                          toupper({{country}}))

        if (is.null({{var}})) {
            caption <- paste0(caption,
                              " - Variables mapped: VLS, VLC, VLnC")

        } else {
            caption <- paste0(caption,
                              " - ",
                              "Variable mapped: ",
                              toupper({{var}}))
        }

        caption <- paste0(caption,
                          ", Produced on ",
                          format(Sys.Date(), "%Y%m%d"))

        return(caption)
    }


#' @title Generate Graphic / Output filename
#'
#' @param country country name
#' @param var df variable
#' @return plot file name
#'
get_output_name <-
    function(country,
             var = "VLC") {

        name <- paste0("FY20Q4_ViralLoad_",
                       toupper({{var}}),
                       "_",
                       toupper({{country}}),
                       "_",
                       format(Sys.Date(), "%Y%m%d"),
                       ".png"
        )

        return(name)
    }



#' Map VL S/C/nC
#'
#' @param spdf PEPFAR Spatial Data
#' @param vl_variable Viral Load Variable
#' @param cntry OU Name
#' @param terr_raster RasterLayer
#' @param peds VL for <15
#' @param caption Add caption to the output?
#' @param save Save the output to ./Graphics folder
#' @param agency Facets indicator by fundingagencies in data frame
#' @facet_rows Number of facet rows to use; default is 1
#' @return ggplot plot of the map
#'
map_viralload <-
    function(df,
             spdf,
             terr_raster,
             vl_variable,
             region = "south-east",
             peds = FALSE,
             caption = TRUE,
             save = FALSE,
             agency = TRUE,
             facet_rows = 1) {

        # Variables
        df_geo <- {{spdf}}
        df_vl <- {{df}}
        vl_var <- {{vl_variable}}
        #terr <- {{terr_raster}}
        peds_title <- {{peds}}

        # Country boundaries
        df_geo0 <- df_geo %>%
            filter(countryname == country, type == "OU")

        # PSNU Geo + VL data
        df_geo2 <- df_geo %>%
            filter(countryname == country, type == "PSNU") %>%
            left_join(df_vl, by = c("uid" = "psnuuid")) %>%
            rename(snu1 = snu1.x) %>%
            filter(!is.na(VLnC))

        if (region == "south-east") {
            df_geo2 <- df_geo2 %>%
                filter(snu1 != "Kinshasa")

            base_map <- se_base
        }
        else if (region == "kinshasa") {
            df_geo2 <- df_geo2 %>%
                filter(snu1 == "Kinshasa")

            base_map <- kin_base
        }


        # Map specific variable
        if (tolower(vl_var) == "vls") {
            theme_map <- base_map +
                geom_sf(
                    data = df_geo2,
                    aes(fill = VLS),
                    lwd = .2,
                    color = grey10k,
                    alpha = 0.8
                ) +
                scale_fill_stepsn(
                    breaks = c(0, .8, .9, 1),
                    guide = guide_colorsteps(even.steps = FALSE),
                    na.value = grey40k,
                    limits = c(0, 1),
                    labels = percent,
                    colors = RColorBrewer::brewer.pal(n = 11, name = "RdYlGn")
                )

            if (agency == TRUE) {
                theme_map <-
                    theme_map + facet_wrap( ~ fundingagency, nrow = facet_rows)
            }

        }
        else if (tolower(vl_var) == "vlc") {
            theme_map <- base_map +
                geom_sf(
                    data = df_geo2,
                    aes(fill = VLC),
                    lwd = .2,
                    color = grey10k
                ) +
                scale_fill_viridis_c(
                    option = "magma",
                    alpha = 0.9,
                    direction = -1,
                    na.value = grey40k,
                    breaks = c(0, .25, .50, .75, 1.00),
                    limits = c(0, 1),
                    labels = percent
                )

            if (agency == TRUE) {
                theme_map <-
                    theme_map + facet_wrap( ~ fundingagency, nrow = facet_rows)
            }
        }
        else {
            theme_map <- base_map +
                geom_sf(
                    data = df_geo2,
                    aes(fill = VLnC),
                    lwd = .2,
                    color = grey10k
                ) +
                scale_fill_viridis_c(
                    option = "viridis",
                    alpha = 0.9,
                    direction = -1,
                    na.value = grey40k,
                    breaks = c(0, .25, .50, .75, 1.00),
                    limits = c(0, 1),
                    labels = percent
                )

            if (agency == TRUE) {
                theme_map <-
                    theme_map + facet_wrap( ~ fundingagency, nrow = facet_rows)
            }
        }

        # Add country boundaries and apply map theme
        theme_map <- theme_map +
            geom_sf(
                data = df_geo0,
                colour = grey90k,
                fill = NA,
                size = 1
            ) +
            si_style_map()


        # Add Caption
        if (caption == TRUE) {
            theme_map <- theme_map +
                labs(
                    title = get_title(var = vl_var, peds = peds_title),
                    caption = get_caption(country, var = vl_var)
                )
        }
        else {
            theme_map <- theme_map +
                labs(title = get_title(var = vl_var, peds = peds_title))
        }

        # Update legend size and position
        theme_map <- theme_map +
            theme(
                legend.position =  "bottom",
                legend.direction = "horizontal",
                legend.key.width = ggplot2::unit(1.5, "cm"),
                legend.key.height = ggplot2::unit(.5, "cm")
            )


        #print(theme_map)


        if (save == TRUE) {
            ggsave(
                here::here("Graphics", get_output_name(country, var = vl_var)),
                plot = last_plot(),
                scale = 1.2,
                dpi = 400,
                width = 10,
                height = 7,
                units = "in"
            )
        }

        return(theme_map)
    }


#'
#' @param df
#' @param vl_variable
#'
plot_viralload <- function(df, vl_variable = "VLC") {

    # Reorder Provinces
    plot <- df %>%
        mutate(snu1 = factor(
            snu1, levels = c("Haut Katanga", "Lualaba", "Kinshasa")))

    # Create plots
    if (vl_variable == "VLC") {
        plot <- df_vl %>%
            ggplot(aes(reorder(psnu, VLC), VLC, fill = VLC)) +
            geom_col(width = .8,
                     position = position_dodge2(width = .8, preserve = "single"),
                     show.legend = F) +
            scale_fill_viridis_c(option = "magma", alpha = .9, direction = -1)

    }
    else if (vl_variable == "VLS") {

        plot <- df_vl %>%
            ggplot(aes(reorder(psnu, VLS), VLC, fill = VLS)) +
            geom_col(show.legend = F) +
            scale_fill_stepsn(
                breaks = c(0, .8, .9, 1),
                guide = guide_colorsteps(even.steps = FALSE),
                na.value = grey40k,
                limits = c(0, 1),
                labels = percent,
                colors = RColorBrewer::brewer.pal(n = 11, name = "RdYlGn")
            )
    }
    else {
        plot <- df_vl %>%
            ggplot(aes(reorder(psnu, VLS), VLC, fill = VLS)) +
            geom_col(show.legend = F) +
            scale_fill_viridis_c(
                option = "viridis",
                alpha = 0.9,
                direction = -1,
                na.value = grey40k,
                breaks = c(0, .25, .50, .75, 1.00),
                limits = c(0, 1),
                labels = percent
            )
    }


    plot <- plot +
        scale_y_continuous(position = "right",
                           limits = c(0, 1),
                           breaks = c(0, .25, .5, .75, 1),
                           labels = percent) +
        coord_flip() +
        facet_grid(snu1 ~ ., scales = "free", space = "free", switch = "both") +
        labs(x = "", y = "") +
        si_style_xgrid() +
        theme(strip.placement = "outside",
              strip.text = element_text(hjust = .5))


    return(plot)
}


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

terr <- get_raster()

## VL
df_vl <- extract_viralload(
    df_msd = df_msd,
    rep_agency = agency,
    rep_fy = curr_fy,
    rep_pd = curr_pd)

df_vl %>% glimpse()
View(df_vl)

## VL PEDS
df_vl_peds <- extract_viralload(
    df_msd = df_msd,
    rep_agency = agency,
    rep_fy = curr_fy,
    rep_pd = curr_pd,
    peds = TRUE)


# VIZ -------------------------------------

spdf_pepfar %>%
    filter(countryname == country, type == "PSNU") %>%
    left_join(df_vl, by = c("uid" = "psnuuid")) %>%
    filter(!is.na(VLnC)) %>%
    st_set_geometry(NULL) %>%
    View()

# VLC

map_se_vlc <- map_viralload(
    spdf = spdf_pepfar,
    df = df_vl,
    vl_variable = "VLC",
    region = "south-east",
    terr_raster = terr,
    caption = F) +
    geom_sf(data = adm1_se, fill = NA, size = .5) +
    geom_sf_text(data = adm1_se, aes(label = province), size = 4) +
    theme(legend.position = "top")
    #labs(caption = vl_caption)


map_kin_vlc <- map_viralload(
    spdf = spdf_pepfar,
    df = df_vl,
    vl_variable = "VLC",
    region = "kinshasa",
    terr_raster = terr,
    caption = F) +
    geom_sf(data = adm1_kin, fill = NA, size = .5) +
    geom_sf_text(data = adm1_kin, aes(label = province), size = 4) +
    theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          strip.text = element_blank(),
          legend.position = "none")

map_vlc <- ggdraw() +
    draw_plot(map_se_vlc) +
    draw_plot(map_kin_vlc, x = .05, y = .10,
              width = .3,
              height = .3) +
    draw_plot(basemap_aoi, x = .4, y = .10,
              width = .2,
              height = .2
    )

map_vlc

ggsave(
    here::here("Graphics", "FY20Q4_DRC_Map_ViralLoad_Coverage.png"),
    plot = last_plot(),
    scale = 1.2,
    dpi = 400,
    width = 10,
    height = 7,
    units = "in"
)

plot_vlc <- plot_viralload(df_vl, "VLC")

plot_vlc

ggsave(
    here::here("Graphics", "FY20Q4_DRC_PLot_ViralLoad_Coverage.png"),
    plot = last_plot(),
    scale = 1.2,
    dpi = 400,
    width = 10,
    height = 7,
    units = "in"
)

vlc <- (map_vlc + plot_vlc) +
    labs(caption = vl_caption)

vlc

ggsave(
    here::here("Graphics", "FY20Q4_DRC_Graph_ViralLoad_Coverage.png"),
    plot = last_plot(),
    scale = 1.2,
    dpi = 400,
    width = 10,
    height = 7,
    units = "in"
)


# VLS
map_se_vls <- map_viralload(
    spdf = spdf_pepfar,
    df = df_vl,
    vl_variable = "VLS",
    region = "south-east",
    terr_raster = terr,
    caption = F) +
    geom_sf(data = adm1_se, fill = NA, size = .5) +
    geom_sf_text(data = adm1_se, aes(label = province), size = 4) +
    theme(legend.position = "top")


map_kin_vls <- map_viralload(
    spdf = spdf_pepfar,
    df = df_vl,
    vl_variable = "VLS",
    region = "kinshasa",
    terr_raster = terr,
    caption = F) +
    geom_sf(data = adm1_kin, fill = NA, size = .5) +
    geom_sf_text(data = adm1_kin, aes(label = province), size = 4) +
    theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          strip.text = element_blank(),
          legend.position = "none")

map_vls <- ggdraw() +
    draw_plot(map_se_vls) +
    draw_plot(map_kin_vls, x = .05, y = .10,
              width = .3,
              height = .3) +
    draw_plot(basemap_aoi, x = .4, y = .10,
              width = .2,
              height = .2
    )

map_vls

ggsave(
    here::here("Graphics", "FY20Q4_DRC_Map_ViralLoad_Suppression.png"),
    plot = last_plot(),
    scale = 1.2,
    dpi = 400,
    width = 10,
    height = 7,
    units = "in"
)

plot_vls <- plot_viralload(df_vl, "VLS")

plot_vls

ggsave(
    here::here("Graphics", "FY20Q4_DRC_PLot_ViralLoad_Suppression.png"),
    plot = last_plot(),
    scale = 1.2,
    dpi = 400,
    width = 10,
    height = 7,
    units = "in"
)

vls <- (map_vls + plot_vls) +
    labs(caption = vl_caption)

vls

ggsave(
    here::here("Graphics", "FY20Q4_DRC_Graph_ViralLoad_Suppression.png"),
    plot = last_plot(),
    scale = 1.2,
    dpi = 400,
    width = 10,
    height = 7,
    units = "in"
)



# VLnC
map_se_vlnc <- map_viralload(
    spdf = spdf_pepfar,
    df = df_vl,
    vl_variable = "VLnC",
    region = "south-east",
    terr_raster = terr,
    caption = F) +
    geom_sf(data = adm1_se, fill = NA, size = .5) +
    geom_sf_text(data = adm1_se, aes(label = province), size = 4) +
    theme(legend.position = "top")


map_kin_vlnc <- map_viralload(
    spdf = spdf_pepfar,
    df = df_vl,
    vl_variable = "VLnC",
    region = "kinshasa",
    terr_raster = terr,
    caption = F) +
    geom_sf(data = adm1_kin, fill = NA, size = .5) +
    geom_sf_text(data = adm1_kin, aes(label = province), size = 4) +
    theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          strip.text = element_blank(),
          legend.position = "none")

map_vlnc <- ggdraw() +
    draw_plot(map_se_vlnc) +
    draw_plot(map_kin_vlnc, x = .05, y = .10,
              width = .3,
              height = .3) +
    draw_plot(basemap_aoi, x = .4, y = .10,
              width = .2,
              height = .2
    )

map_vlnc

ggsave(
    here::here("Graphics", "FY20Q4_DRC_Map_ViralLoad_Not-Coverage.png"),
    plot = last_plot(),
    scale = 1.2,
    dpi = 400,
    width = 10,
    height = 7,
    units = "in"
)

plot_vlnc <- plot_viralload(df_vl, "VLnC")

plot_vlnc

ggsave(
    here::here("Graphics", "FY20Q4_DRC_PLot_ViralLoad_Not-Coverage.png"),
    plot = last_plot(),
    scale = 1.2,
    dpi = 400,
    width = 10,
    height = 7,
    units = "in"
)

vlnc <- (map_vlnc + plot_vlnc) +
    labs(caption = vl_caption)

vlnc

ggsave(
    here::here("Graphics", "FY20Q4_DRC_Graph_ViralLoad_Not-Coverage.png"),
    plot = last_plot(),
    scale = 1.2,
    dpi = 400,
    width = 10,
    height = 7,
    units = "in"
)



