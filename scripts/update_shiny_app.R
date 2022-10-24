### Script that updates the data needed for the ABF Observations Explorer App
### Only needs to be updated once per year after the festival is completed

## Source functions needed
source("scripts/abf_functions.R")


## Run data cleaning
rawdata <- read.csv("data/abf_raw_data_2022.csv")

clean_data(rawdata)


## Summarize and write out necessary data
data <- read.csv("outputs/abf_clean_data.csv")

get_summaries(data)
