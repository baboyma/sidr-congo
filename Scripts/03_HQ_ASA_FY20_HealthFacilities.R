## PROJECT:  SI Support for DRC
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  DRC/PEPFAR Health Zones
## Date:     2020-11-20

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

# SOURCE ----------------------------------

source("./Scripts/00_Config.R")

# GLOBAL ----------------------------------

country <- "Democratic Republic of the Congo"

# Files

file_fac <- list.files(
    path = dir_dataout,
    pattern = country,
    full.names = TRUE
)

file_fac

# DATA

df_facilities <- file_fac %>%
    vroom(col_types = c(.default = "c")) %>%
    filter(!is.na(longitude), !is.na(latitude)) %>%
    mutate_at(.vars = vars(ends_with("tude")), .funs = as.numeric)

df_facilities %>% glimpse()
df_facilities %>% View()


# VIZ
basemap_aoi +
    geom_point(data = df_facilities,
               aes(x = longitude, y = latitude),
               color = USAID_dkred) +
    labs(title = "DRC - PEPFAR Supported Health Facilities",
         subtitle = "Sites without valid coordinates have note been displayed",
         caption = paste0("OHA/SIEI - Strategic Information Branch
         Data source: DATIM, Produced on ", format(Sys.Date(), "%Y%m%d"))) +
    theme(text = element_text(family = "Gill Sans MT"),
          plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5))


# ggsave(here("./Graphics", "DRC_Health_Facilities_Locations.png"),
#        plot = last_plot(), scale = 1.2, dpi = 310,
#        width = 10, height = 7, units = "in")


#