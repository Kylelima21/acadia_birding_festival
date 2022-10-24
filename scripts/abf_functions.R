require(tidyverse)
require(leaflet)
require(lubridate)



#' Function to clean the raw eBird data. 
#'
#' @param x: The raw data to be cleaned.
#' @export

clean_data <- function(x) {
  
  clean <- tibble(x) %>% 
    rename_with(tolower) %>% 
    select(common.name, scientific.name, count, submission.id, location.id, location, 
           latitude, longitude, date, time, duration.min = duration..min., complete = all.obs.reported, 
           distance.km = distance.traveled..km., num.observers = number.of.observers) %>% 
    mutate(date = mdy(date),
           url = paste0("https://ebird.org/checklist/", submission.id),
           count = str_replace(count, "x", "1"),
           count = str_replace(count, "X", "1"),
           count = as.numeric(count),
           scientific.name = str_replace(scientific.name, " x \\w*", ""),
           scientific.name = str_replace(scientific.name, "\\/\\w*", ""),
           scientific.name = str_extract(scientific.name, "^\\w*\\s\\w*"),
           scientific.name = str_replace(scientific.name, " sp$", " sp."),
           common.name = str_remove(common.name, "\\s\\(.*$"),
           common.name = str_replace(common.name, "Mallard/American Black Duck", "duck sp."),
           common.name = str_replace(common.name, "Mallard x American Black Duck", "duck sp."),
           common.name = str_replace(common.name, "Short-billed/Long-billed Dowitcher", "shorebird sp."),
           common.name = str_replace(common.name, "Common/Arctic Tern", "Sterna sp."),
           common.name = str_replace(common.name, "Downy/Hairy Woodpecker", "woodpecker sp."),
           common.name = str_replace(common.name, "Golden-winged x Blue-winged Warbler", "warbler sp."),
           common.name = str_replace(common.name, "Alder/Willow Flycatcher", "Empidonax sp."),
           scientific.name = ifelse(common.name == "Empidonax sp." & scientific.name == 
                                      "Empidonax alnorum", "Empidonax sp.", scientific.name),
           scientific.name = ifelse(common.name == "duck sp." & scientific.name == 
                                      "Anas platyrhynchos", "duck sp.", scientific.name),
           scientific.name = ifelse(common.name == "shorebird sp." & scientific.name == 
                                      "Limnodromus griseus", "shorebird sp.", scientific.name),
           scientific.name = ifelse(common.name == "Sterna sp." & scientific.name == 
                                      "Sterna hirundo", "Sterna sp.", scientific.name),
           scientific.name = ifelse(common.name == "warbler sp." & scientific.name == 
                                      "Vermivora chrysoptera", "warbler sp.", scientific.name),
           scientific.name = ifelse(common.name == "woodpecker sp." & scientific.name == 
                                      "Dryobates pubescens", "woodpecker sp.", scientific.name)
    ) %>% 
    replace(is.na(.), 0) %>% 
    mutate(trip = ifelse(grepl("Beech Mtn.", location) | 
                           grepl("Beech Mountain", location), "Beech Mountain", NA),
           trip = ifelse(grepl("Blagden", location), "Blagden Preserve", trip),
           trip = ifelse(grepl("Northeast Creek", location), "Canoe and Bird - Northeast Creek", trip),
           trip = ifelse(grepl("Clark Point Road", location), "Clark Point Road", trip),
           trip = ifelse(grepl("Essex Woods", location), "Essex Woods in Bangor", trip),
           trip = ifelse(location == "Somesville" | location == "Mill Pond, Somesville", "Fish Ladders in Somesville", trip),
           trip = ifelse(grepl("Frenchboro", location), "Frenchboro Preserve on Long Island", trip),
           trip = ifelse(grepl("Long Pond", location) &
                           !grepl("Pretty Marsh", location), "Long Pond Loop", trip),
           trip = ifelse(grepl("Monhegan", location), "Monhegan Island Trip", trip),
           trip = ifelse(grepl("Otter Point", location), "Otter Point", trip),
           trip = ifelse(grepl("pelagic", location, ignore.case = T) |
                           grepl("Petit Manan Island", location) |
                           grepl("Mt. Desert Rock", location), "Pelagic Seabird Boat Trip", trip),
           trip = ifelse(grepl("Precipice Trail", location), "Peregrine Falcon Viewing", trip),
           trip = ifelse(grepl("Hollingsworth Trail", location) |
                           grepl("Petit Manan Point Rd", location), "Petit Manan NWR", trip),
           trip = ifelse(grepl("Pretty Marsh", location), "Pretty Marsh/Long Pond Fire Road", trip),
           trip = ifelse(grepl("Breakneck", location) |
                           grepl("Witch Hole Pond Carriage", location), "Rockefeller Carriage Road", trip),
           trip = ifelse(grepl("Saddleback", location), "Saddleback Mountain - Bicknell's Thrush", trip),
           trip = ifelse(grepl("Schoodic Peninsula", location) |
                           grepl("Schoodic Point", location) |
                           grepl("SERC Campus", location) | 
                           grepl("Schoodic Institute Campus", location) |
                           grepl("Acadia NP--Blueberry Hill", location) |  
                           grepl("Schoodic Pt- Blueberry Hill", location) &
                           !grepl("pelagic", location, ignore.case = T), "Schoodic Peninsula", trip),
           trip = ifelse(grepl("Seal Cove", location), "Seal Cove - Cape Rd", trip),
           trip = ifelse(grepl("Sears Island", location), "Sears Island", trip),
           trip = ifelse(grepl("Ship Harbor Trail", location), "Ship Harbor Nature Trail", trip),
           trip = ifelse(grepl("Sieur de Monts", location), "Sieur de Monts Spring", trip),
           trip = ifelse(grepl("Swans Island", location), "Swans Island", trip),
           trip = ifelse(grepl("Valley Cove", location), "Valley Cove and Flying Mountain", trip),
           trip = ifelse(grepl("Wonderland Trail", location), "Wonderland", trip))
  
  
  # Write out the cleaned data!
  write.csv(clean, "outputs/abf_clean_data.csv")
  write.csv(clean, "app/www/datasets/abf_clean_data.csv")

}




#' Function to produce some summaries from the raw eBird data. 
#'
#' @param x: The cleaned data for summarizing.
#' @export

get_summaries <- function(x) {
  
  num_checklists <- x %>% 
    select(submission.id) %>% 
    distinct() %>% 
    tally() %>% 
    mutate(category = "num_checklists")
  # unlist() %>% 
  # unname()
  
  num_locs <- x %>% 
    select(location.id) %>% 
    distinct() %>% 
    tally() %>% 
    mutate(category = "num_locations")
  
  num_birds <- x %>% 
    select(count) %>% 
    summarise(n = sum(count)) %>%
    mutate(category = "num_individuals")
  
  num_species <- x %>% 
    select(common.name, scientific.name) %>% 
    filter(!grepl(" sp.$", scientific.name)) %>% 
    distinct() %>%
    tally() %>% 
    mutate(category = "num_species")
  
  summary_one <- bind_rows(num_checklists, num_locs, num_birds, num_species) %>% 
    select(category, total = n) %>% 
    arrange(total)
  
  add_in <- data.frame(year = c(2007, 2008, 2009, 2020),
                       total = c(rep(0, 4)))
  
  total_species_year <- x %>% 
    select(common.name, scientific.name, date) %>% 
    filter(!grepl(" sp.$", common.name)) %>% 
    group_by(year(date)) %>% 
    select(-date, year = `year(date)`) %>% 
    distinct() %>% 
    tally() %>% 
    mutate(year = as.integer(year)) %>% 
    rename(total = n) %>% 
    bind_rows(add_in) %>% 
    arrange(year)
  
  
  all_taxa_totals_yearly <- x %>% 
    select(common.name, scientific.name, date, count) %>% 
    #filter(!grepl(" sp.$", scientific.name)) %>%
    group_by(year(date), scientific.name, common.name) %>% 
    summarise(total.count = sum(count), .groups = "drop") %>% 
    rename(year = `year(date)`) %>% 
    select(common.name, scientific.name, year, total.count) %>% 
    pivot_wider(., values_from = total.count, names_from = year) %>% 
    replace(is.na(.), 0) %>% 
    arrange(common.name) %>% 
    rowwise() %>%
    mutate(total = sum(across(starts_with("20"))))
  
  
  write.csv(summary_one, "outputs/number_summary.csv", row.names = F)
  write.csv(total_species_year, "outputs/species_per_year.csv", row.names = F)
  write.csv(all_taxa_totals_yearly, "outputs/all_taxa_summary.csv", row.names = F)
  write.csv(summary_one, "app/www/datasets/number_summary.csv", row.names = F)
  write.csv(total_species_year, "app/www/datasets/species_per_year.csv", row.names = F)
  write.csv(all_taxa_totals_yearly, "app/www/datasets/all_taxa_summary.csv", row.names = F)
  
}




#' Function to produce an interactive leaflet map widget of observations
#'
#' @return A leaflet map widget of recent observations.
#' @param x: A data frame of observations.
#' @export

make_leaflet <- function (x) {
  
  formap <- x %>% 
    mutate(#url = paste0("<center><b><a href='", url, "'>View observation<br>online</a></b></center>"),
           latitude = jitter(latitude, factor = 16),
           longitude = jitter(longitude, factor = 16)) 
  
  maxLong = max(formap$longitude) - 0.05
  maxLat = max(formap$latitude) + 0.05 
  minLong = min(formap$longitude) + 0.05
  minLat = min(formap$latitude) - 0.05
  
  map <- leaflet() %>% 
    addProviderTiles(providers$Esri.WorldImagery) %>% 
    addProviderTiles(providers$Stamen.TerrainLabels) %>% 
    addMarkers(formap$longitude, formap$latitude, label = formap$common.name,
               popup = formap$url, #options = popupOptions(minWidth = 600),
               labelOptions = labelOptions(textsize = "15px")) %>%
    fitBounds(minLong, minLat, maxLong, maxLat)
  
  
  return(map)
}




#' Function to produce an interactive leaflet map widget of observations
#'
#' @return A leaflet map widget of recent observations.
#' @param x: A data frame of observations.
#' @export

leaflet_summary <- function (x) {
  
  formap <- x #%>% 
  #   mutate(url = paste0("<b><a href='", url, "'>View observation<br>on eBird</a></b>")) 
  
  maxLong = max(formap$longitude) + 1
  maxLat = max(formap$latitude)
  minLong = min(formap$longitude)
  minLat = min(formap$latitude) - 0.05
  
  map <- leaflet() %>% 
    addProviderTiles(providers$Esri.WorldImagery) %>% 
    addProviderTiles(providers$Stamen.TonerLines) %>% 
    addProviderTiles(providers$Stamen.TerrainLabels) %>% 
    addMarkers(formap$longitude, formap$latitude, clusterOptions = markerClusterOptions()) %>%
    fitBounds(minLong, minLat, maxLong, maxLat)
  
  return(map)
}








