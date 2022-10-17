### Script that updates the data needed for the ABF Observations Explorer App
### Only needs to be updated once per year after the festival is completed

## Source functions needed
source("scripts/abf_functions.R")

## Run the data cleaning script
source("scripts/01_data_manipulation.R")

## Summarize and write out necessary data
data <- read.csv("outputs/abf_clean_data.csv")

leaflet_summary(data)