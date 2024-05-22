# Function to map SPI Birds derived abundance data on breeding pairs to Darwin Core Archive with occurrence core ####

# Author: Cherine Jantzen
# Created: 2024-05-15
# Last updated: 2024-05-22


# I. Preparation ----------------------------------------------------------

## load packages
library(dplyr)
library(taxize)
library(stringr)
library(tidyr)

# II. Function ------------------------------------------------------------

# Arguments

## countryCode: character, two letter ISO country code specifying the country in which the data was collected
## institution: character, specifying the name of the institution that owns the data
## institutionID: character, specifying the institional ID as required for DwC-term "institutionID". Should ideally be Research Organization Registry (ROR) identifier (e.g., "https://ror.org/01g25jp36")
## quantityType: character, specifying the system or unit in which the organism Quantities are given (i.e. DwC-term "organismQuantityType"). Default is set to "breeding pairs" as the function is tailored to breeding pair count data
## data_directory: character, specifying the name of the folder or directory the output files should be stored in
## output_prefix: character, specifying the prefix of the filename for the output csv file

map_to_DwCA <- function(countryCode,
                        institution,
                        institutionID,
                        quantityType = "breeding pairs",
                        data_directory,
                        output_prefix) {
  
  # get supportive information to translate species and location codes from SPI Birds & get coordinates of study sites
  species_codes <- read.csv("https://raw.githubusercontent.com/SPI-Birds/pipelines/master/inst/extdata/species_codes.csv")
  pop_codes <- read.csv("https://raw.githubusercontent.com/SPI-Birds/pipelines/master/inst/extdata/pop_codes.csv")
  pop_locations <- read.csv("https://raw.githubusercontent.com/SPI-Birds/pipelines/master/inst/extdata/pop_locations.csv")
  
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
    dplyr::mutate(taxonRank = "species") %>% 
    dplyr::rename("vernacularName" = "CommonName")
  
  # B. Location information ----
  
  ## translate population ID into verbatim names and get coordinates per area
  locationInformation <- brood %>% 
    dplyr::distinct(., PopID, .keep_all = FALSE) %>% 
    dplyr::left_join(pop_codes %>% 
                       dplyr::select("PopID", "PopName"), 
                     by = "PopID") %>% 
    dplyr::mutate(verbatimLocality = dplyr::case_when(PopID == "LIC" ~ "Lichtenbeek", 
                                                      PopID == "LIE" ~ "Liesbosch Breda", 
                                                      TRUE ~ PopName)) %>% 
    dplyr::left_join(pop_locations %>% 
                       dplyr::select("site_name", "country", "latitude", "longitude") %>% 
                       dplyr::mutate(site_name = dplyr::case_when(stringr::str_detect(string = site_name, pattern = "Westerheide") ~ "Westerheide",
                                                                  TRUE ~ site_name),
                                     countryCode = countryCode),
                     by = c("verbatimLocality" = "site_name")) %>% 
    dplyr::rename(decimalLatitude = latitude,
                  decimalLongitude = longitude) %>% 
    dplyr::mutate(geodeticDatum = "EPSG:4326")
  
  # C. Get Event information ----
  
  # event date range
  eventInformation <- brood %>% 
    dplyr::filter(ClutchType_calculated == "first") %>% 
    dplyr::group_by(PopID, Species, BreedingSeason) %>% 
    dplyr::summarise(minLD = min(LayDate_min, na.rm = TRUE),
                     maxLD = max(LayDate_max, na.rm = TRUE)) %>% 
    dplyr::mutate(eventDate = dplyr::if_else(!is.na(minLD), paste(minLD, substring(maxLD, first = 6, last = 10), sep = "/"), as.character(BreedingSeason))) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(location %>% 
                       dplyr::distinct(., PopID, HabitatType, .keep_all = FALSE),
                     by = "PopID") %>% 
    # add metadata fields of event class
    dplyr::mutate(language = "en",
                  institutionID = institutionID,
                  institutionCode = institution)
  
  # D. Combine information into occurrence core file ----
  occurrence <- breedingPairs %>% 
    # add taxonomic information
    dplyr::left_join(taxonInformation %>% 
                       dplyr::select("SpeciesID", "kingdom", "phylum", "class", "order", "family", "genus", 
                                     "specificEpithet", "scientificName" = "scientificname", "taxonRank", "vernacularName"),
                     by = c("Species" = "SpeciesID")) %>% 
    # add location information
    dplyr::left_join(locationInformation %>% 
                       dplyr::select(!"PopName"), 
                     by = "PopID") %>% 
    # add event information
    dplyr::left_join(eventInformation %>% 
                       dplyr::select(!c("minLD", "maxLD")), 
                     by = c("PopID", "Species", "BreedingSeason")) %>% 
    dplyr::rename("year" = "BreedingSeason",
                  "organismQuantity" = "n") %>% 
    # add missing terms
    dplyr::mutate(basisOfRecord = "HumanObservation",
                  organismQuantityType = quantityType,
                  occurrenceStatus = "present",
                  occurrenceID = paste(PopID, year, Species, sep = "-")) %>% 
      # reorder columns for the final output file
    dplyr::select("occurrenceID", "year", "eventDate", "organismQuantity", "organismQuantityType", "occurrenceStatus", "basisOfRecord",
                  "country", "countryCode", "verbatimLocality", "decimalLatitude", "decimalLongitude", "geodeticDatum", "language", 
                  "institutionID", "institutionCode", "scientificName", "kingdom", "phylum", "class", "order", "family", "genus", 
                  "specificEpithet", "taxonRank", "vernacularName")
  
  # save additional information to be used in EML file
  save(taxonInformation, file = paste(data_directory, "taxonInformation.rda", sep = "/"))
  save(locationInformation, file = paste(data_directory, "locationInformation.rda", sep = "/"))
  
  # save occurrence output file
  # write occurrence file
  write.csv(occurrence, file = paste(data_directory, paste(output_prefix, "occurrence.csv", sep = "_"), sep = "/"), row.names = FALSE)
  
  return(occurrence)
  
}
