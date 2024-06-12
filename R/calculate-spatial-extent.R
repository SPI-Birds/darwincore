# Functions to calculate the spatial extent (in meters) of a study location and map locations on an interactive Leaflet map ####

# Authors: Stefan Vriend
# Created: 2024-06-04
# Last updated: 2024-06-12


# Load packages -----------------------------------------------------------

library(dplyr)
library(geosphere)
library(leaflet)

# Function ----------------------------------------------------------------

# Arguments:
# - brood: R data object containing SPI-Birds formatted brood data
# - location: R data object containing SPI-Birds formatted location data

calculate_spatial_extent <- function(brood,
                                     location) {
  
  # Keep location records that are associated with broods
  location <- location |> 
    dplyr::filter(LocationID %in% brood$LocationID)
  
  # Calculate the four margins of the spatial bounding box  (S, N, W, E) 
  extent <- location |>
    dplyr::summarise(minLat = min(Latitude, na.rm = TRUE),
                     maxLat = max(Latitude, na.rm = TRUE),
                     minLon = min(Longitude, na.rm = TRUE),
                     maxLon = max(Longitude, na.rm = TRUE),
                     .by = "PopID") |> 
    # Temporary fix: add bounding box for Liesbos and Warnsborn manually
    # until they are corrected in SPI-Birds
    dplyr::mutate(minLat = dplyr::case_when(PopID == "LIE" ~ 51.58148,
                                            PopID == "WAR" ~ 52.00952,
                                            TRUE ~ minLat),
                  maxLat = dplyr::case_when(PopID == "LIE" ~ 51.58474,
                                            PopID == "WAR" ~ 52.01781,
                                            TRUE ~ maxLat),
                  minLon = dplyr::case_when(PopID == "LIE" ~ 4.68757,
                                            PopID == "WAR" ~ 5.85353,
                                            TRUE ~ minLon),
                  maxLon = dplyr::case_when(PopID == "LIE" ~ 4.69770,
                                            PopID == "WAR" ~ 5.87227,
                                            TRUE ~ maxLon))
  
  # Extract centre points from pop_codes (a SPI-Birds internal table)
  centroids <- pop_codes |> 
    dplyr::filter(PopID %in% extent$PopID) |> 
    dplyr::select(PopID, Latitude, Longitude)
  
  # Calculate the distances between the margins and centre points.
  # The maximum distance describes the smallest circle containing all relevant locations,
  # and provides a good measure of dwc:coordinateUncertaintyInMeters
  dplyr::left_join(extent, centroids, by = "PopID") |> 
    dplyr::rowwise(PopID) |> # Because geosphere::distHaversine() is not vectorised
    dplyr::mutate(west = geosphere::distHaversine(p1 = c(minLon, Latitude),
                                                  p2 = c(Longitude, Latitude)),
                  east = geosphere::distHaversine(p1 = c(maxLon, Latitude),
                                                  p2 = c(Longitude, Latitude)),
                  north = geosphere::distHaversine(p1 = c(Longitude, maxLat),
                                                   p2 = c(Longitude, Latitude)),
                  south = geosphere::distHaversine(p1 = c(Longitude, minLat),
                                                   p2 = c(Longitude, Latitude))) |>
    # Round the uncertainty to the nearest 100 meter
    dplyr::mutate(uncertainty = round(max(c(west, east, north, south)) / 100) * 100,
                  uncertainty = dplyr::if_else(uncertainty == 0, NA_real_, uncertainty)) |> 
    dplyr::select("PopID", 
                  "coordinateUncertaintyInMeters" = "uncertainty")
  
}

# Example
# calculate_spatial_extent(brood, location)


# Function to verify locations on an interactive map ----------------------

# Arguments:
# - PopID: SPI-Birds identifier of study site 
# - brood: R data object containing SPI-Birds formatted brood data
# - location: R data object containing SPI-Birds formatted location data

map_locations <- function(PopID,
                          brood,
                          location) {
  
  # Keep location records that are associated with broods
  location <- location |> 
    dplyr::filter(LocationID %in% brood$LocationID)
  
  # Map individual location records to map
  # Check for potentially erroneous records
  leaflet::leaflet() |> 
    leaflet::fitBounds(lng1 = min(location[location$PopID == PopID, ]$Longitude),
                       lng2 = max(location[location$PopID == PopID, ]$Longitude),
                       lat1 = min(location[location$PopID == PopID, ]$Latitude),
                       lat2 = max(location[location$PopID == PopID, ]$Latitude)) |> 
    leaflet::addTiles() |> 
    leaflet::addCircleMarkers(lng = location[location$PopID == PopID, ]$Longitude,
                              lat = location[location$PopID == PopID, ]$Latitude)
  
}

# Example
# map_locations("HOG", brood, location)
