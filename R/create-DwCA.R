# NIOO SPI Birds data to Darwin Core Archive ####

# Author: Cherine Jantzen, Stefan Vriend
# Created: 2024-05-13
# Last updated: 2024-06-12


# I. Preparation ----------------------------------------------------------

## load packages
library(dplyr)
library(here)

## get data from csv files
brood <- read.csv(file.choose()) # Brood_data
location <- read.csv(file.choose()) # Location_data

## get functions to create DwC-A
source(here::here("R","function-map-to-DwC-A.R"))
source("https://raw.githubusercontent.com/LTER-LIFE/FDFDT/main/R/create-meta-xml-of-DwCA.R")

# retrieve location information for each population from SPI Birds
pop_codes <- read.csv("https://raw.githubusercontent.com/SPI-Birds/pipelines/master/inst/extdata/pop_codes.csv")

# II. Data processing ---------------------------------------------------------

## calculate species abundance as breeding pairs based on number of first clutches using ClutchType_calculated
breedingPairs <- brood %>% 
  dplyr::filter(ClutchType_calculated == "first") %>% 
  dplyr::group_by(PopID, BreedingSeason, Species) %>% 
  dplyr::count() %>% 
  dplyr::ungroup()

# III. Map to Darwin Core -------------------------------------------------

map_to_DwCA(protocol = "Visser, M. E., Lindner, M., Gienapp, P., Long, M. C., & Jenouvrier, S. (2021). 
            Recent natural variability in global warming weakened phenological mismatch and selection on seasonal timing in great tits (Parus major). 
            Proceedings of the Royal Society B, 288(1963), 20211337. https://doi.org/10.1098/rspb.2021.1337", 
            institution = "NIOO-KNAW",
            institutionID = "https://ror.org/01g25jp36",
            data_directory = "data",
            output_prefix = "SPIBirds") %>% 
  purrr::walk2(.x = c("event", "occurrence"),
               .y = .,
               .f = ~ {
                 
                 assign(.x, .y, envir = .GlobalEnv)
                 
               })
  

# IV. Meta-xml file -------------------------------------------------------

# create meta.xml file for beech crop DwC-A
create_meta_xml(core = c("Event" = here::here("data", "SPIBirds_event.csv")),
                extensions = c("Occurrence" = here::here("data", "SPIBirds_occurrence.csv")),
                file = here::here("data", ("SPIBirds_meta.xml")))
