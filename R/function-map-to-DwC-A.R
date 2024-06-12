# Function to map SPI Birds derived abundance data on breeding pairs to Darwin Core Archive with event core and occurrence extension ####

# Authors: Cherine Jantzen, Stefan Vriend
# Created: 2024-05-15
# Last updated: 2024-06-04


# I. Preparation ----------------------------------------------------------

## load packages
library(dplyr)
library(taxize)
library(stringr)
library(tidyr)
library(lubridate)

# II. Function ------------------------------------------------------------

# Arguments

## protocol: character, specifying the sampling protocol used to collect the data
## institution: character, specifying the name of the institution that owns the data
## institutionID: character, specifying the institional ID as required for DwC-term "institutionID". Should ideally be Research Organization Registry (ROR) identifier (e.g., "https://ror.org/01g25jp36")
## quantityType: character, specifying the system or unit in which the organism Quantities are given (i.e. DwC-term "organismQuantityType"). Default is set to "breeding pairs" as the function is tailored to breeding pair count data
## data_directory: character, specifying the name of the folder or directory the output files should be stored in
## output_prefix: character, specifying the prefix of the filename for the output csv file

map_to_DwCA <- function(protocol,
                        institution,
                        institutionID,
                        quantityType = "breeding pairs",
                        data_directory,
                        output_prefix) {
  
  # get supportive information to translate species and location codes from SPI Birds & get coordinates of study sites
  species_codes <- read.csv("https://raw.githubusercontent.com/SPI-Birds/pipelines/master/inst/extdata/species_codes.csv")
  # pop_codes <- read.csv("https://raw.githubusercontent.com/SPI-Birds/pipelines/master/inst/extdata/pop_codes.csv")
  
  # get function to calculate the geographic uncertainty
  source(here::here("R", "calculate-spatial-extent.R"))
  
  # A. Taxonomic information ----
  
  ## find unique species in the data and translate species codes to binomial names
  taxa <- tibble::tibble(SpeciesID = unique(breedingPairs$Species)) %>%
    dplyr::left_join(species_codes %>% 
                       dplyr::select("Species", "BinomialName", "CommonName"), 
                     by = c("SpeciesID" = "Species"))
  
  ## query taxonomic information from GBIF
  taxonInformation <- taxize::get_gbifid_(sci = taxa$BinomialName) %>% 
    dplyr::bind_rows() %>% 
    dplyr::filter(status == "ACCEPTED", matchtype == "EXACT") %>% 
    dplyr::left_join(taxa, by = c("canonicalname" = "BinomialName")) %>% 
    tidyr::separate(canonicalname, c("Genus", "specificEpithet"), remove = FALSE) %>% 
    dplyr::mutate(taxonRank = "species",
                  taxonomicStatus = tolower(status),
                  genericName = genus) %>% 
    dplyr::rename("vernacularName" = "CommonName")
  
  # B. Location information ----
  ## calculate coordinate uncertainty
  coordinateUncertainty <- calculate_spatial_extent(brood, location)
  
  ## get location information from SPI-Birds file `pop_codes`
  locationInformation <- brood %>% 
    dplyr::distinct(PopID, .keep_all = FALSE) %>% 
    dplyr::left_join(pop_codes %>% 
                       dplyr::select("PopID", "PopName", "Country", "CountryCode", "Latitude", "Longitude"), 
                     by = "PopID") %>% 
    dplyr::rename(country = Country,
                  countryCode = CountryCode,
                  verbatimLocality = PopName) %>% 
    dplyr::mutate(geodeticDatum = "EPSG:4326",
                  continent = "Europe",
                  decimalLatitude = round(Latitude, digits = 5),
                  decimalLongitude = round(Longitude, digits = 5)) %>% 
    dplyr::left_join(coordinateUncertainty, by = "PopID")
  
  # C. Get Event information ----
  
  # calculate number of nest boxes sampled per year-location to take as sampleSizeValue
  sampleSize <- brood %>% 
    dplyr::filter(ClutchType_calculated == "first") %>% 
    dplyr::group_by(PopID, Species, BreedingSeason) %>% 
    dplyr::distinct(LocationID, .keep_all = TRUE) %>% 
    dplyr::summarise(sampleSizeValue = dplyr::n()) %>% 
    dplyr::ungroup()
  
  # event date range
  eventInformation <- brood %>% 
    dplyr::filter(ClutchType_calculated == "first") %>% 
    dplyr::group_by(PopID, Species, BreedingSeason) %>% 
    dplyr::summarise(minLD = min(c(LayDate_min, LayDate_observed), na.rm = TRUE),
                     maxLD = max(c(LayDate_max, LayDate_observed), na.rm = TRUE)) %>% 
    dplyr::mutate(eventDate = paste(minLD, maxLD, sep = "/"),
                  startDayOfYear = lubridate::yday(substring(eventDate, first = 1, last = 10)),
                  endDayOfYear = lubridate::yday(substring(eventDate, first = 12, last = 21))) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(sampleSize, by = c("PopID", "BreedingSeason", "Species")) %>% 
    dplyr::rename("year" = "BreedingSeason") %>% 
      # add metadata fields of event class
    dplyr::mutate(language = "en",
                  institutionID = institutionID,
                  institutionCode = institution,
                  samplingProtocol = protocol, 
                  sampleSizeUnit = "nest boxes",
                  type = "Event",
                  eventID = paste(PopID, year, Species, sep = "-"))
  
  # D. Combine information into event core & occurrence extension file ----
  
  ## Event core
  event <- eventInformation %>% 
    # add location information
    dplyr::left_join(locationInformation, by = "PopID") %>% 
    dplyr::select("eventID", "samplingProtocol", "sampleSizeValue", "sampleSizeUnit", "eventDate", "startDayOfYear", 
                  "endDayOfYear", "year",  "continent", "country", "countryCode", "decimalLatitude","decimalLongitude",
                  "geodeticDatum", "coordinateUncertaintyInMeters", "verbatimLocality", "type", "language", "institutionID", "institutionCode")
  
  
  ## Occurrence extension
  occurrence <- breedingPairs %>% 
    left_join(eventInformation %>%  
                dplyr::select("PopID", "year", "Species", "eventID"),
              by = c("PopID", "BreedingSeason" = "year", "Species")) %>% 
    # add taxonomic information
    dplyr::left_join(taxonInformation %>%
                       dplyr::select("SpeciesID", "kingdom", "phylum", "class", "order", "family", "genus",
                                     "specificEpithet", "scientificName" = "scientificname", "taxonRank", "vernacularName", "genericName", "taxonomicStatus"),
                     by = c("Species" = "SpeciesID")) %>%
    dplyr::rename("organismQuantity" = "n") %>% 
    # add missing terms
    dplyr::mutate(basisOfRecord = "HumanObservation",
                  organismQuantityType = quantityType,
                  occurrenceStatus = "present",
                  occurrenceID = paste(eventID, 1:dplyr::n(), sep = "_"), .by = eventID) %>% 
    # reorder columns for the final output file
    dplyr::select("eventID", "occurrenceID", "organismQuantity", "organismQuantityType", "occurrenceStatus", "basisOfRecord",      
                  "scientificName", "kingdom", "phylum", "class", "order", "family", "genus", 
                  "genericName", "specificEpithet", "taxonRank", "vernacularName", "taxonomicStatus")
  

  # E. Save output files ----------------------------------------------------

  # save additional information to be used in EML file
  save(taxonInformation, file = paste(data_directory, "taxonInformation.rda", sep = "/"))
  save(locationInformation, file = paste(data_directory, "locationInformation.rda", sep = "/"))

  # write event file
  write.csv(event, file = paste(data_directory, paste(output_prefix, "event.csv", sep = "_"), sep = "/"),
            row.names = FALSE)
  
  # write occurrence file
  write.csv(occurrence, file = paste(data_directory, paste(output_prefix, "occurrence.csv", sep = "_"), sep = "/"),
            row.names = FALSE)
  
  
  
  return(list(event, occurrence))
  
}
