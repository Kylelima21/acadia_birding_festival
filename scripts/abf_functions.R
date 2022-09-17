require(tidyverse)
require(lubridate)
require(leaflet)
# require(shiny)
# require(shinythemes)
# require(shinyWidgets)
# require(bslib)
# require(fresh)
# require(png)
# require(grid)
# require(purrr)
# require(sp)
# require(rgdal)

## List of functions



#' Function returns summaries and a data frame of recent iNaturalist observations 
#'
#' This function takes a recent time span and returns all iNaturalist records from
#' inside a desired location during that time span. Additionally, this 
#' function produces four data frames with summary statistics from the iNaturalist data.
#'
#' @inheritParams None
#' @return A data frame of recent iNaturalist observations.
#' @param timespan: The recent time span of interest. Options 
#' are "week", "threedays", or "yesterday" as inputs for the "timespan" parameter.
#' @param output.path: The path you want the summary statistic tables to be written to.
#' @param place: An iNaturalist slug (the place name) as a single string with words separated by hyphens. 
#' For example, the place Acadia National Park would be "acadia-national-park" as found in the place page
#' url: https://www.inaturalist.org/observations?place_id=142267
#' @seealso None
#' @export
#' @examples  
#' example_data <- inat_recent("acadia-national-park", "week", "outputs/inat_summary")
















#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 