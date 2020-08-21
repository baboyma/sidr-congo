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

# FILES

list.files(
        path = "./_secrets",
        pattern = ".R$",
        full.names = TRUE
    ) %>%
    source()
