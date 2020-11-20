## PROJECT:  SI Support for DRC
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Configurations
## Date:     2020-08-03

# LIBRARIES -------------------------------

library(tidyverse)
library(here)

# DIRECTORIES

dir_data <- "Data"
dir_dataout <- "Dataout"

dir_images <- "Images"
dir_graphics <- "Graphics"

dir_docs <- "Documents"

dir_merdata <- "../../MERDATA"
dir_geodata <- "../../GEODATA/PEPFAR"
dir_raster <- "../../GEODATA/RASTER"

# FILES

list.files(
        path = "./_secrets",
        pattern = ".R$",
        full.names = TRUE
    ) %>%
    source()



#' @title Get Terrain Raster dataset
#'
#' @param terr_path path to terrain raster file
#' @return RasterLayer
#'
get_raster <-
    function(terr_path = "../../GEODATA/RASTER",
             name = "SR_LR.tif") {

        # Params
        dir_terr <- {{terr_path}}
        file_name <- {{name}}

        # Check directory
        if (!dir.exists(dir_terr))
            stop(cat("\nInvalid terrain directory: ",
                     paint_red(dir_terr),
                     "\n"))

        # Identify file path
        terr_file <- list.files(
            path = dir_terr,
            pattern = paste0(file_name, "$"),
            recursive = TRUE,
            full.names = TRUE
        )

        # Check file
        if (!file.exists(terr_file))
            stop(cat("\nFile does not exist: ",
                     terr_file,
                     "\n"))

        # Read file content
        ras <- raster::raster(terr_file)

        return(ras)
    }



#' @title Get basemap
#'
#' @param spdf PEPFAR ORGs Spatial Data
#' @param cntry OU or Country Name
#' @param terr_raster RasterLayer
#' @param add_admins Should the sub-admins be added? Default is false
#' @return ggplot plot of base map
#'
get_basemap <-
    function(admin0, terr_raster,
             admin1 = NULL,
             admin2 = NULL) {



        # Transform geodata
        spdf_admin0 <- admin0 %>%
            sf::st_as_sf() %>%
            sf::st_transform(., crs = sf::st_crs(4326)) %>%
            sf::st_zm()

        # Admin 1
        if (!is.null(admin1))
            spdf_admin1 <- admin1 %>%
                sf::st_as_sf() %>%
                sf::st_transform(., crs = sf::st_crs(4326)) %>%
                sf::st_zm()

        # Admin 2
        if (!is.null(admin2))
            spdf_admin2 <- admin2 %>%
                sf::st_as_sf() %>%
                sf::st_transform(., crs = sf::st_crs(4326)) %>%
                sf::st_zm()


        # Crop
        terr <- terr_raster %>%
            raster::crop(x = ., y = raster::extend(raster::extent(spdf_admin0), .2)) %>%
            raster::mask(x = ., mask = spdf_admin0)

        # Convert raster data into a spatial data frame
        trdf <- terr %>%
            as("SpatialPixelsDataFrame") %>%
            as.data.frame() %>%
            dplyr::rename(value = SR_LR) %>%
            dplyr::filter(value < 210)

        # Basemap
        m <- ggplot() +
            geom_tile(data = trdf,
                      aes(x, y, alpha = value)) +
            scale_alpha(name = "",
                        range = c(0.6, 0),
                        guide = F) +
            geom_sf(
                data = spdf_admin0,
                colour = "white",
                fill = grey10k,
                size = 2,
                alpha = .25
            )

        # Add sub-admins boundaries
        if (!is.null(admin2)) {
            m <- m +
                geom_sf(
                    data = spdf_admin2,
                    fill = "NA",
                    linetype = "dotted",
                    size = .3
                )
        }

        # Add sub-admins boundaries
        if (!is.null(admin1)) {
            m <- m +
                geom_sf(
                    data = spdf_admin1,
                    fill = "NA",
                    size = .8
                )
        }

        # Add country boundary
        m <- m +
            geom_sf(
                data = spdf_admin0,
                colour = grey90k,
                fill = "NA",
                size = 1
            ) +
            si_style_map()

        return(m)
    }

