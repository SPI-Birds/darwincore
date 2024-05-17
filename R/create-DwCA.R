# NIOO SPI Birds data to Darwin Core Archive ####

# Author: Cherine Jantzen
# Created: 2024-05-13
# Last updated: 2024-05-17


# I. Preparation ----------------------------------------------------------

## load packages
library(dplyr)

## get data from csv files
brood <- read.csv(file.choose()) # Brood_data
location <- read.csv(file.choose()) # Location_data

## get functions to create DwC-A
source(here::here("R","function-map-to-DwC-A.R"))
source("https://raw.githubusercontent.com/LTER-LIFE/FDFDT/main/R/create-meta-xml-of-DwCA.R")

# II. Data processing ---------------------------------------------------------

## calculate species abundance as breeding pairs based on number of first clutches using ClutchType_calculated
breedingPairs <- brood %>% 
  dplyr::filter(ClutchType_calculated == "first") %>% 
  dplyr::group_by(PopID, BreedingSeason, Species) %>% 
  dplyr::count() %>% 
  dplyr::ungroup()

# III. Map to Darwin Core -------------------------------------------------

occurrence <- map_to_DwCA(countryCode = "NL",
                          institution = "NIOO-KNAW",
                          institutionID = "https://ror.org/01g25jp36",
                          output_prefix = "SPIBirds")

# IV. Meta-xml file -------------------------------------------------------

# create meta.xml file for beech crop DwC-A
create_meta_xml(core = c("Occurrence" = here::here("data", "SPIBirds_occurrence.csv")),
                file = here::here("data", ("SPIBirds_meta.xml")))
