## Schoodic Institute at Acadia National Park
## Acadia Birding Festival

# Script to manipulate the raw data

#-------------------------------------------#
####           Load packages             ####
#-------------------------------------------#

library(tidyverse)
library(lubridate)



#-------------------------------------------#
####         Clean and modify            ####
#-------------------------------------------#

# Read in and clean/modify the raw data
clean <- tibble(read.csv("data/abf_raw_data.csv")) %>% 
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
  replace(is.na(.), 0)


# Write out the cleaned data!
write.csv(clean, "outputs/abf_clean_data.csv")
write.csv(clean, "app/www/datasets/abf_clean_data.csv")



