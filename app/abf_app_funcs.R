require(tidyverse)
require(leaflet)
require(lubridate)
require(shiny)
require(shinythemes)
require(shinyWidgets)
require(echarts4r)
require(fullPage)
require(ggplot2)
require(viridis)
require(scales)


## List of functions
# get_summaries()
# add_external_resources()
# leaflet_summary()
# species_leaflet()
# spyplot()
# sp_trends()

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
    group_by(year(date), scientific.name, common.name) %>% 
    summarise(total.count = sum(count), .groups = "drop") %>% 
    rename(year = `year(date)`) %>% 
    select(common.name, scientific.name, year, total.count) %>% 
    pivot_wider(., values_from = total.count, names_from = year) %>% 
    replace(is.na(.), 0) %>% 
    arrange(common.name) %>% 
    rowwise() %>%
    mutate(total = sum(across(starts_with("20"))))
  
  ## Write out summary info
  write.csv(summary_one, "outputs/number_summary.csv", row.names = F)
  write.csv(total_species_year, "outputs/species_per_year.csv", row.names = F)
  write.csv(all_taxa_totals_yearly, "outputs/all_taxa_summary.csv", row.names = F)
  
}




#' Function to produce an interactive leaflet map widget of observations
#'
#' @return A leaflet map widget of recent observations.
#' @param x: A data frame of observations.
#' @export

add_external_resources <- function() {
  
  tags$head(
    includeCSS("www/css/styles.css"),
    golem::activate_js())
  
}




#' Function to produce an interactive leaflet map widget of observations
#'
#' @return A leaflet map widget of recent observations.
#' @param x: A data frame of observations.
#' @export

leaflet_summary <- function (x) {
  
  formap <- x %>% 
    mutate(url = paste0("<b><a href='", url, "' target='_blank' rel='noopener noreferrer'", ">View observation<br>on eBird </a></b>")) 
  
  maxLong = max(formap$longitude) + 1
  maxLat = max(formap$latitude)
  minLong = min(formap$longitude)
  minLat = min(formap$latitude)
  
  map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
    addProviderTiles(providers$Esri.WorldImagery) %>% 
    addProviderTiles(providers$Stamen.TonerLines, options = providerTileOptions(opacity = 0.35)) %>% 
    #addProviderTiles(providers$Stamen.TerrainLabels) %>%
    addProviderTiles(providers$CartoDB.PositronOnlyLabels) %>% 
    addMarkers(formap$longitude, formap$latitude, label = formap$common.name,
               labelOptions = labelOptions(textsize = "15px"),
               clusterOptions = markerClusterOptions(),
               popup = formap$url) %>%
    fitBounds(minLong, minLat, maxLong, maxLat)
  
  return(map)
}




#' Function to produce an interactive leaflet map widget of observations
#'
#' @return A leaflet map widget of recent observations.
#' @param x: A data frame of observations.
#' @export

species_leaflet <- function (x) {
  
  formap <- x %>% 
    mutate(url = paste0("<b><a href='", url, "' target='_blank' rel='noopener noreferrer'", ">View checklist<br>on eBird </a></b>")) 
    #mutate(url = paste0("<b><a href='", url, "'>View observation<br>on eBird</a></b>")) 
  
  maxLong = max(formap$longitude) + 1
  maxLat = max(formap$latitude)
  minLong = min(formap$longitude)
  minLat = min(formap$latitude) + 0.2
  
  map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
    addProviderTiles(providers$Esri.WorldImagery) %>% 
    addProviderTiles(providers$Stamen.TonerLines, options = providerTileOptions(opacity = 0.35)) %>% 
    #addProviderTiles(providers$Stamen.TerrainLabels) %>%
    addProviderTiles(providers$CartoDB.PositronOnlyLabels) %>% 
    addMarkers(formap$longitude, formap$latitude, label = formap$location,
               labelOptions = labelOptions(textsize = "15px"),
               clusterOptions = markerClusterOptions(),
               popup = formap$url) %>%
    fitBounds(minLong, minLat, maxLong, maxLat)
  
  return(map)
}




#' Function to produce a graphic of species trends by year
#'
#' @return A graphic displaying species trends by year
#' @param x: A data frame of observations.

sp_trends <- function(x) {
  
  plot <- x %>% 
    select(common.name, scientific.name, date, count) %>% 
    group_by(year(date), scientific.name, common.name) %>% 
    summarise(count = sum(count), .groups = "drop") %>% 
    rename(year = `year(date)`) %>% 
    select(common.name, scientific.name, year, count) %>% 
    filter(year > 2006) %>% 
    arrange(common.name, year) %>% 
    ggplot(aes(as.factor(year), common.name, fill = count)) + 
    geom_tile(color = "gray10", size = 1.5) + 
    scale_fill_viridis(option = "viridis") +
    scale_x_discrete(breaks = factor(2010:2022), limits = c("2010", "2011", "2012", "2013", "2014",
                                                            "2015", "2016", "2017", "2018", "2019",
                                                            "2020", "2021", "2022")) +
    xlab("") + 
    ylab("") +
    ggtitle("Total Individuals Recorded per Year") +
    theme(plot.title = element_text(color = "white", hjust = 0.5, vjust = 0.8, size = rel(2)),
          plot.background = element_rect(fill = "gray10", color = "gray10"),
          plot.margin = unit(c(14.5, 18.5, 5.5, 5.5), "points"),
          panel.background = element_rect(fill = "gray10"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          axis.ticks.y = element_blank(), 
          axis.text = element_text(color = "white", size = rel(1.3)),
          # axis.text.y  = element_text(hjust = 1),
          axis.text.y  = element_blank(),
          legend.text = element_text(color = "white", size = rel(1.3)),
          legend.background = element_rect(fill = "gray10"),
          legend.position = "bottom",
          legend.key.size = unit(.5, "cm"),
          legend.key.width = unit(2, "cm"),
          legend.title = element_blank())
  
  return(plot)
}




#' Function to produce a graph of the number of trips/year
#'
#' @return A bar graph of yearly observations
#' @param x: A data frame of observations.
#' @export

partypy_plot <- function(x) {
  
  plot <- x %>% 
    mutate(year = as.character(year)) %>% 
    filter(year > 2009) %>% 
    ggplot(aes(year, participants, fill = participants)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = participants), vjust = -1, size = 5) +
    scale_y_continuous(expand = c(0,0), limits = c(0,416)) +
    scale_x_discrete(breaks = x$year[seq(1, length(x$year), by = 2)]) +
    scale_fill_viridis(option = "cividis",
                       values = rescale(c(0, 140, max(x$participants))),
                       n.breaks = 4) +
    labs(x = "Year", y = "Total participants", title = "Number of Participants Each Year") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.key.size = unit(1, "cm"),
          legend.text = element_text(color = "black", size = 14),
          axis.text = element_text(color = "black", size = 14),
          axis.title = element_text(color = "black", size = 16),
          plot.title = element_text(size = 24, hjust = 0.5),
          plot.background = element_rect(fill = "gray97", color = "black", size = 2),
          panel.background = element_rect(fill = "gray97"),
          legend.background = element_rect(fill = "gray97"))
  
  return(plot)
  
}


#' Function to produce a graph of total species observed/year
#'
#' @return A bar graph of yearly observations
#' @param x: A data frame of observations.
#' @export

spy_plot <- function(x) {
  
  plot <- x %>% 
    mutate(year = as.character(year)) %>% 
    filter(year > 2009) %>% 
    ggplot(aes(year, total, fill = total)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = total), vjust = -1, size = 5) +
    scale_y_continuous(expand = c(0,0), limits = c(0,220)) +
    scale_x_discrete(breaks = x$year[seq(1, length(x$year), by = 2)]) +
    scale_fill_viridis(option = "mako",
                       values = rescale(c(0, 140, max(x$total))),
                       n.breaks = 4) +
    labs(x = "Year", y = "Total species", title = "Number of Species Observed Each Year") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.key.size = unit(1, "cm"),
          legend.text = element_text(color = "black", size = 14),
          axis.text = element_text(color = "black", size = 14),
          axis.title = element_text(color = "black", size = 16),
          plot.title = element_text(size = 24, hjust = 0.5),
          plot.background = element_rect(fill = "gray97", color = "black", size = 2),
          panel.background = element_rect(fill = "gray97"),
          legend.background = element_rect(fill = "gray97"))
  
  return(plot)
  
}




#' Function to produce a graph of total birds observed/year
#'
#' @return A bar graph of yearly observations
#' @param x: A data frame of observations.
#' @export

tpy_plot <- function(x) {
  
  data <- x %>% 
    mutate(year = year(date)) %>% 
    group_by(year) %>% 
    summarise(count = sum(count), .groups = "drop") %>% 
    filter(year > 2009) %>% 
    mutate(year = as.character(year))
  
  plot <- data %>% 
    ggplot(aes(year, count, fill = count)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = comma(count)), vjust = -1.5, hjust = 0.5, size = 5, angle = 30) +
    scale_y_continuous(expand = c(0,0), limits = c(0,20100)) +
    scale_x_discrete(breaks = data$year[seq(1, length(data$year), by = 2)]) +
    scale_fill_viridis(option = "viridis",
                       values = rescale(c(0, 140, max(data$count))),
                       n.breaks = 4) +
    labs(x = "Year", y = "Total individuals", title = "Total Number of Birds Counted Each Year") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.key.size = unit(1, "cm"),
          legend.text = element_text(color = "black", size = 14),
          axis.text = element_text(color = "black", size = 14),
          axis.title = element_text(color = "black", size = 16),
          plot.title = element_text(size = 24, hjust = 0.5),
          plot.background = element_rect(fill = "gray97", color = "black", size = 2),
          panel.background = element_rect(fill = "gray97"),
          legend.background = element_rect(fill = "gray97"))
  
  return(plot)
}




#' Function to produce a graph of the number of trips/year
#'
#' @return A bar graph of yearly observations
#' @param x: A data frame of observations.
#' @export

trippy_plot <- function(x) {
  
  plot <- x %>% 
    mutate(year = as.character(year)) %>% 
    filter(year > 2009) %>% 
    ggplot(aes(year, trips, fill = trips)) +
    # geom_point() +
    # geom_smooth(method = "lm", color = "black", se = F) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = trips), vjust = -1, size = 5) +
    scale_y_continuous(expand = c(0,0), limits = c(0,110)) +
    scale_x_discrete(breaks = x$year[seq(1, length(x$year), by = 2)]) +
    scale_fill_viridis(option = "rocket",
                       values = rescale(c(0, 40, max(x$trips))),
                       n.breaks = 4,
                       direction = 1) +
    labs(x = "Year", y = "Total trips", title = "Number of Trips Offered Each Year") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.key.size = unit(1, "cm"),
          legend.text = element_text(color = "black", size = 14),
          axis.text = element_text(color = "black", size = 14),
          axis.title = element_text(color = "black", size = 16),
          plot.title = element_text(size = 24, hjust = 0.5),
          plot.background = element_rect(fill = "gray97", color = "black", size = 2),
          panel.background = element_rect(fill = "gray97"),
          legend.background = element_rect(fill = "gray97"))
  
  return(plot)
  
}




#' Function to produce a table to summarize trips offered by ABF
#'
#' @return A table summarizing all events from an offered ABF trip
#' @param x: A data frame of observations.
#' @export

trip_summary <- function(x) {
  
  if(unique(x$trip) == "Schoodic Peninsula") {
    
    dat <- x %>% 
      filter(num.observers < 100) %>% 
      select(common.name:location.id, date, num.observers)
    
  } else {
    
    dat <- x %>% 
      select(common.name:location.id, date, num.observers)
    
  }
  
  
  output <- data.frame(`Number_of_species` = length(unique(dat$scientific.name)),
                       `Number_of_checklists` = length(unique(dat$submission.id)),
                       `Number_of_years_run` = length(unique(year(dat$date))),
                       `Average_number_of_participants` = round(mean(dat$num.observers), 0)) %>% 
    pivot_longer(`Number_of_species`:`Average_number_of_participants`) %>% 
    rename(Category = name, Value = value)
  
  
  return(output)
}




#' Function to produce an interactive leaflet map widget of trip locations
#'
#' @return A leaflet map widget of trip locations.
#' @param x: A data frame of observations.
#' @export

trip_leaflet <- function (x) {
  
  formap <- x

  maxLong = max(formap$trip.long) + 0.8
  maxLat = max(formap$trip.lat) + 0.8
  minLong = min(formap$trip.long) - 0.8
  minLat = min(formap$trip.lat) - 0.8
  
  map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
    addProviderTiles(providers$Esri.WorldImagery) %>% 
    addProviderTiles(providers$Stamen.TonerLines, options = providerTileOptions(opacity = 0.35)) %>% 
    addProviderTiles(providers$CartoDB.PositronOnlyLabels) %>% 
    addMarkers(formap$trip.long, formap$trip.lat)%>%
    fitBounds(minLong, minLat, maxLong, maxLat)
  
  return(map)
}
