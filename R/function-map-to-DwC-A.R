# Function to map SPI Birds derived abundance data on breeding pairs to Darwin Core Archive with occurrence core ####

# Authors: Cherine Jantzen, Stefan Vriend
# Created: 2024-05-15
# Last updated: 2024-05-30


# I. Preparation ----------------------------------------------------------

## load packages
library(dplyr)
library(taxize)
library(stringr)
library(tidyr)

# II. Function ------------------------------------------------------------

# Arguments

## institution: character, specifying the name of the institution that owns the data
## institutionID: character, specifying the institional ID as required for DwC-term "institutionID". Should ideally be Research Organization Registry (ROR) identifier (e.g., "https://ror.org/01g25jp36")
## quantityType: character, specifying the system or unit in which the organism Quantities are given (i.e. DwC-term "organismQuantityType"). Default is set to "breeding pairs" as the function is tailored to breeding pair count data
## data_directory: character, specifying the name of the folder or directory the output files should be stored in
## output_prefix: character, specifying the prefix of the filename for the output csv file

map_to_DwCA <- function(institution,
                        institutionID,
                        quantityType = "breeding pairs",
                        data_directory,
                        output_prefix) {
  
  # get supportive information to translate species and location codes from SPI Birds & get coordinates of study sites
  species_codes <- read.csv("https://raw.githubusercontent.com/SPI-Birds/pipelines/master/inst/extdata/species_codes.csv")
  pop_codes <- read.csv("https://raw.githubusercontent.com/SPI-Birds/pipelines/master/inst/extdata/pop_codes.csv")
  
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
                  decimalLongitude = round(Longitude, digits = 5))
  
  # C. Get Event information ----
  
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
    # add metadata fields of event class
    dplyr::mutate(language = "en",
                  institutionID = institutionID,
                  institutionCode = institution)
  
  # D. Combine information into occurrence core file ----
  occurrence <- breedingPairs %>% 
    # add taxonomic information
    dplyr::left_join(taxonInformation %>%
                       dplyr::select("SpeciesID", "kingdom", "phylum", "class", "order", "family", "genus",
                                     "specificEpithet", "scientificName" = "scientificname", "taxonRank", "vernacularName", "genericName", "taxonomicStatus"),
                     by = c("Species" = "SpeciesID")) %>%
    # add location information
    dplyr::left_join(locationInformation, 
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
    dplyr::select("occurrenceID", "year", "eventDate", "startDayOfYear", "endDayOfYear", "organismQuantity", "organismQuantityType", "occurrenceStatus", "basisOfRecord", "continent",
                  "country", "countryCode", "verbatimLocality", "decimalLatitude", "decimalLongitude", "geodeticDatum", "language", 
                  "institutionID", "institutionCode", "scientificName", "kingdom", "phylum", "class", "order", "family", "genus", 
                  "genericName", "specificEpithet", "taxonRank", "vernacularName", "taxonomicStatus")
  
  # save additional information to be used in EML file
  save(taxonInformation, file = paste(data_directory, "taxonInformation.rda", sep = "/"))
  save(locationInformation, file = paste(data_directory, "locationInformation.rda", sep = "/"))

  # save occurrence output file
  # write occurrence file
  write.csv(occurrence, file = paste(data_directory, paste(output_prefix, "occurrence.csv", sep = "_"), sep = "/"),
            row.names = FALSE)
  
  return(occurrence)
  
}
