require(tidyverse)
require(leaflet)
require(lubridate)
require(shiny)
require(shinythemes)
require(shinyWidgets)
library(echarts4r)
# require(bslib)
# require(fresh)
# require(png)
# require(grid)
# require(purrr)
# require(sp)
# require(rgdal)

## List of functions
# get_summaries()
# make_leaflet()
# make_summary()


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
  
  map <- leaflet() %>% 
    addProviderTiles(providers$Esri.WorldImagery) %>% 
    addProviderTiles(providers$Stamen.TonerLines) %>% 
    addProviderTiles(providers$Stamen.TerrainLabels) %>% 
    addMarkers(formap$longitude, formap$latitude, clusterOptions = markerClusterOptions())
  
  return(map)
}








