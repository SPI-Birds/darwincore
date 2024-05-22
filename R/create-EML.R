# Pipeline for creation of EML metadata file of the DwC-A of the SPI Birds NIOO data ####

# Authors: Cherine Jantzen
# Created: 2024-05-14
# Last updated: 2024-05-22


# Load packages
library(emld)
library(EML)
library(xml2)
library(here)

# now the script requires the taxonInformation object from the mapping script
load(here::here("data", "taxonInformation.rda"))
load(here::here("data", "locationInformation.rda"))


# I. Fill in metadata ----------------------------------------------------

# Title of the data set
title <- "Breeding pair abundances derived from nestbox studies of the Netherlands Institute of Ecology (NIOO-KNAW) as stored at SPI-Birds"

# Information on the creator of the data set
creator <- list(individualName = list(givenName = "Marcel",
                                      surName = "Visser"),
                organizationName = "Department of Animal Ecology, Netherlands Institute of Ecology (NIOO-KNAW)",
                address = list(country = "NL",
                               city = "Wageningen"),
                electronicMailAddress = "AnE_Database@nioo.knaw.nl",
                userId = "0000-0002-1456-1939")

# Information on the provider of the meta data
metadataProvider <- list(individualName = list(givenName = "Cherine",
                                               surName = "Jantzen"),
                         organizationName = "Netherlands Institute of Ecology (NIOO-KNAW)",
                         address = list(country = "NL",
                                        city = "Wageningen"),
                         electronicMailAddress = "c.jantzen@nioo.knaw.nl",
                         userId = "0009-0006-0723-2682")

# Information on the contact person
contact_person <- list(organizationName = "SPI-Birds",
                       address = list(country = "NL", # FIXME Where?
                                      city = "Wageningen"), # FIXME Where?
                       positionName = "?", # FIXME Who?
                       electronicMailAddress = "spibirds@nioo.knaw.nl") # FIXME correct?

# Language of the data
language <- "en"

# Abstract describing the data set
abstract <- list(para = "??")

                   
# List of keywords and the thesaurus they are listed in
keywords <- list(list(keyword = list("breeding birds", "ecology", "bird species abundance", "phenology"),
                      keywordThesaurus = "envThes"),
                 list(keyword = list("nest box", "passerine")))

# License for the work
licensed <- list(licenseName = "Creative Commons Attribution 4.0 International (CC BY 4.0)",
                 url = "https://creativecommons.org/licenses/by/4.0/")

# geographic coverage
geographic_coverage <- list(geographicDescription = "Eight study sites across the Netherlands, namely Hoge Veluwe, Vlieland, Liesbosch Breda, Westerheide, Buunderkamp, Lichtenbeek, Oosterhout and Warnsborn.",
                            boundingCoordinates = list(westBoundingCoordinate = as.character(min(locationInformation$decimalLongitude)),
                                                       eastBoundingCoordinate = as.character(max(locationInformation$decimalLongitude)),
                                                       northBoundingCoordinate = as.character(max(locationInformation$decimalLatitude)),
                                                       southBoundingCoordinate = as.character(min(locationInformation$decimalLatitude))))
# Temporal coverage of the data
temporal_coverage <- list(rangeOfDates = list(beginDate = list(calendarDate = "1955"),
                                              endDate = list(calendarDate = "2023")))

# Taxonomic coverage of the data
taxonomicClassification <- purrr::map(.x = 1:nrow(taxonInformation),
                                      .f = ~{
                                        
                                        taxonList <- list(taxonRankName = "Species",
                                                          taxonRankValue = taxonInformation$species[.x],
                                                          taxonId = taxonInformation$specieskey[.x],
                                                          commonName = taxonInformation$vernacularName[.x])
                                      })

taxonomic_coverage <- list(generalTaxonomicCoverage = "Data is collected for 8 bird species.",
                           taxonomicClassification = taxonomicClassification)

# Combine all three types of coverage
coverage <- list(geographicCoverage = geographic_coverage,
                 temporalCoverage = temporal_coverage,
                 taxonomicCoverage = taxonomic_coverage) 

# Methods for data collection
methods <- list(methodStep = list(list(description = list(para = "At each study site, nest boxes were checked at least once a week. Breeding parameters (such as laying date, clutch type and clutch size) were recorded and adults were captured in the nest box. Adults are identified based on their ring number and newly ringed if they do not have a ring yet.")),
                                  list(description = list(para = "This data is transformed into the SPI-Birds standard format through a SPI-Birds pipeline. ....")),
                                  list(description = list(para = "From the data in the SPI-Birds format, the number of breeding pairs per year, location and species is calculated. This is done by counting the number of first clutches. First clutches refer to the first clutch laid by a female in that season and are defined based on strict decision rules (see SPI-Birds Standard Protocol https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf)."))))

# Maintenance: frequency of updates
maintenance <- list(maintenanceUpdateFrequency = "unknown",
                    description = list(para = "Frequency of data updates is unknown."))

# Project information
project <- list(title = "SPI-Birds",
                personnel = list(individualName = list(givenName = "name",
                                                       surName = "name"),
                                 organizationName = "organisation",
                                 address = list(country = "country",
                                                city = "city"),
                                 electronicMailAddress = "spibirds@nioo.knaw.nl",
                                 role = "person"))


# Create EML file ---------------------------------------------------------

packageId <- "d8370484-e2bd-483d-a1c5-9a87fc625793"

# Combine all components in one list
eml <- list(dataset =
              list(title = title,
                   creator = creator,
                   metadataProvider = metadataProvider,
                   language = language,
                   abstract = abstract,
                   keywordSet = keywords,
                   licensed = licensed,
                   coverage = coverage,
                   maintenance = maintenance,
                   contact = contact_person,
                   methods = methods,
                   project = project),
            system = "uuid",
            packageId = packageId)

# Write EMl file
EML::write_eml(eml, file = here::here("data", "SPIBirds_EML.xml"))


# 3. Add attributes for specific nodes ------------------------------------

# Read EML file as XML file
EML <- xml2::read_xml(here::here("data", "SPIBirds_EML.xml"))

# Identify all taxonId nodes for which attribute shall be set
taxonId_node <- xml2::xml_find_all(EML, xpath = "//taxonId")

# Set "provider" attribute for taxonId nodes
xml2::xml_set_attr(taxonId_node, attr = "provider", value = "https://www.gbif.org/")

# Identify title node
title_node <- xml2::xml_find_first(EML, xpath = "//title")

# Set title attribute
xml2::xml_set_attr(title_node, attr = "xml:lang", value = "en")

# Identify userId node
userId_node <- xml2::xml_find_all(EML, xpath = "//userId")

# Set directory attribute
xml2::xml_set_attr(userId_node, attr = "directory", value = "https://orcid.org")


# 4. Validate EML file ----------------------------------------------------
if(!emld::eml_validate(EML)) {
  
  stop("The generated EML is not schema-valid.")
  
}

# Write final EML file
xml2::write_xml(EML, file = here::here("data", "SPIBirds_EML.xml"))
