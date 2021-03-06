## PROJECT:  SI Support for DRC
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  DRC/PEPFAR Health Zones
## Date:     2020-11-16

# LIBRARIES -------------------------------

library(tidyverse)
library(cowplot)
library(vroom)
library(janitor)
library(raster)
library(sf)
library(sp)
library(gisr)
library(glitr)
library(here)
library(patchwork) # use of inset_element
library(extrafont)


# CONFIGS --------------------------------

    source("./Scripts/00_Config.R")

# SOURCE ---------------------------------

    source("../lastmile/Scripts/00_Geo_Utilities.R")

# GLABALS --------------------------------

    country <- "Democratic Republic of the Congo"

    iso <- "DRC"

    provs <- c("Kinshasa", "Lualaba", "Haut-Katanga")

    towns <- c("Kasaji", "Kipushi (ville) ")

# GEODATA -------------------------------------

    ## Terrain data

    terr <- get_raster()

    ## Admins

    # Country
    adm0 <- raster::getData(
            name = "GADM",
            country = country,
            level = 0,
            path = here(dir_data, "vector")
        ) %>%
        st_as_sf() %>%
        st_zm(drop = TRUE) %>%
        clean_names() %>%
        rename(name = name_0)


    # Provinces
    adm1 <- raster::getData(
            name = "GADM",
            country = country,
            level = 1,
            path = here(dir_data, "vector")
        ) %>%
        st_as_sf() %>%
        st_zm(drop = TRUE) %>%
        clean_names() %>%
        dplyr::select(province = name_1)


    ## Territories
    adm2 <- raster::getData(
            name = "GADM",
            country = country,
            level = 2,
            path = here(dir_data, "vector")
        ) %>%
        st_as_sf() %>%
        st_zm(drop = TRUE) %>%
        clean_names() %>%
        dplyr::select(province = name_1, territory = name_2, type = engtype_2)


    ## PEPFAR/DRC Health Zones
    hzones <- list.files(
            path = dir_geodata,
            pattern = "Health_Zones_DRC_.*.shp",
            recursive = TRUE,
            full.names = TRUE
        ) %>%
        sort() %>%
        last() %>%
        read_sf() %>%
        st_zm(drop = TRUE) %>%
        clean_names() %>%
        dplyr::select(province, territoire, name)

    # hzones %>%
    #     filter(province == provs[1]) %>%
    #     ggplot() +
    #     geom_sf(aes(fill = territoire), show.legend = F) +
    #     si_style_map()


    # Sub regions

    ## Kinshasa
    adm1_kin <- adm1 %>%
        filter(province == provs[1])

    adm1_kin_bb <- adm1_kin %>%
        st_bbox() %>%
        st_as_sfc()

    adm2_kin <- adm2 %>%
        filter(province == provs[1])

    hzones_kin <- hzones %>%
        filter(province == provs[1])

    ## Kasaji
    adm2_kas <- adm2 %>%
        filter(territory == "Kasaji")

    ## Kipushi (ville)
    adm2_kip <- adm2 %>%
        filter(territory == "Kipushi (ville) ")

    ## South East
    adm1_se <- adm1 %>%
        filter(province %in% provs[2:3])

    adm1_se_bb <- adm1_se %>%
        st_bbox() %>%
        st_as_sfc()

    adm2_se <- adm2 %>%
        filter(province %in% provs[2:3])

    adm2_se2 <- adm2 %>%
        filter(province %in% provs[2:3], !territory %in% towns)

    adm0_se <- adm1_se %>% summarise()

    ## Area of Interest
    adm1_aoi <- adm1 %>%
        filter(province %in% provs)

    adm2_aoi <- adm2 %>%
        filter(province %in% provs)



# VIZ --------------------------------------------------------------

    ## Country Basemap
    basemap <- gisr::terrain_map(countries = country,
                                 adm0 = adm0,
                                 adm1 = adm1,
                                 terr_path = dir_raster,
                                 mask = TRUE)


    # Basemap with lbls
    basemap_lbl <- basemap +
        geom_sf_text(data = adm1, aes(label = province), size = 2.5)

    #basemap_lbl

    # ggsave(here("./Graphics", "DRC_Basemap_with_labels.png"),
    #        plot = last_plot(), scale = 1.2, dpi = 310,
    #        width = 10, height = 7, units = "in")


    # AOI without Labels
    basemap_aoi <- basemap +
        geom_sf(data = adm1_aoi, fill = USAID_blue, alpha = .4) +
        geom_sf(data = adm0, fill = NA, lwd = .5)

    #basemap_aoi

    # ggsave(here("./Graphics", "DRC_Basemap_aoi_without_labels.png"),
    #        plot = last_plot(), scale = 1.2, dpi = 310,
    #        width = 10, height = 7, units = "in")


    # AOI with Labels
    basemap_aoi_lbl <- basemap +
        geom_sf(data = adm1_aoi, fill = USAID_blue, alpha = .4) +
        geom_sf(data = adm0, fill = NA, lwd = .5) +
        geom_sf_text(data = adm1_aoi, aes(label = province), size = 3)

    #basemap_aoi_lbl

    # ggsave(here("./Graphics", "DRC_Basemap_aoi_with_labels.png"),
    #        plot = last_plot(), scale = 1.2, dpi = 310,
    #        width = 10, height = 7, units = "in")


    # AOI
    basemap_aoi_box <- adm1 %>%
        ggplot() +
        geom_sf(fill = NA, lwd = .1) +
        #geom_sf(data = adm2, fill = NA, linetype = "dotted", lwd = .2) +
        #geom_sf(data = adm1_aoi, fill = NA, lwd = .3) +
        geom_sf(data = adm0, fill = NA, lwd = .5) +
        geom_sf(data = adm1_kin_bb, fill = NA, color = "red", size = .3) +
        geom_sf(data = adm1_se_bb, fill = NA, color = "blue", size = .3) +
        #geom_sf_text(data = adm1_aoi, aes(label = province), size = 2.5) +
        si_style_map()


    #basemap_aoi_box

    ## Kinshasa only
    kin <- adm2_kin %>%
        st_zm(drop = TRUE) %>%
        ggplot() +
        geom_sf(aes(fill = type),
                linetype = "dotted", lwd = .4,
                show.legend = FALSE) +
        geom_sf(data = adm1_kin, fill = NA, lwd = .7) +
        geom_sf_text(aes(label = territory), size = 2.5) +
        si_style_map()

    kin


    ## Kas only
    kas <- adm2_kas %>%
        ggplot() +
        geom_sf(aes(fill = type), lwd = .7,
                show.legend = FALSE) +
        geom_sf_text(aes(label = territory), size = 2.5) +
        si_style_map()

    kas


    ## Kip only
    kip <- adm2_kip %>%
        ggplot() +
        geom_sf(aes(fill = type), lwd = .7,
                show.legend = FALSE) +
        geom_sf_text(aes(label = territory), size = 2.5) +
        si_style_map()

    kip

    ## se only
    se_area <- adm2_se %>%
        ggplot() +
        geom_sf(aes(fill = type),
                linetype = "dotted", lwd = .4,
                show.legend = FALSE) +
        geom_sf(data = adm1_se, fill = NA, lwd = .7) +
        geom_sf_text(aes(label = territory), size = 3) +
        si_style_map()

    se_area


    # SE Area Basemap
    se_base <- create_basemap(terr_raster = terr,
                              admin0 = adm1_se,
                              admin1 = adm2_se)


    #se_base

    # SE Basemap with labels
    se_base_lbl <- se_base +
        geom_sf_text(data = adm2_se,
                     aes(label = territory), size = 3)

    #se_base_lbl

    # Kin Area Basemap
    kin_base <- create_basemap(terr_raster = terr,
                               admin0 = adm1_kin,
                               admin1 = adm2_kin)

    #kin_base

    ## Kin Area Basemap
    kin_base_lbl <- kin_base +
        geom_sf_text(data = adm2_kin,
                     aes(label = territory), size = 4)

    #kin_base_lbl


    ## PEPFAR AOI Map
    basemap_pepfar <- ggdraw() +
        draw_plot(se_base) +
        draw_plot(kin_base, x = .05, y = .05,
                  width = .3,
                  height = .3) +
        draw_plot(basemap_aoi, x = .4, y = .05,
                  width = .2,
                  height = .2
        )

    #basemap_pepfar

    # ggsave(here("./Graphics", "DRC_Basemap_pepfar_with_labels.png"),
    #        plot = last_plot(), scale = 1.2, dpi = 310,
    #        width = 10, height = 7, units = "in")


    # Map composition
    map <- ggdraw() +
        draw_plot(se_area) +
        draw_plot(kin, x = .05, y = .05,
                  width = .3,
                  height = .3) +
        draw_plot(kas, x = .315, y = .05,
                  width = .2,
                  height = .2) +
        draw_plot(kip, x = .5, y = .05,
                  width = .2,
                  height = .2) +
        draw_plot(base, x = .3, y = .7,
                  width = .275,
                  height = .275
        )





