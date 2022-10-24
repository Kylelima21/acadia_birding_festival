source("scripts/abf_functions.R")
#source("app/abf_app_funcs.R")

data <- read.csv("outputs/abf_clean_data.csv")

#get_summaries(data)
leaflet_summary(data)


nd <- data %>% 
  filter(trip == "Schoodic Peninsula")



trip_summary <- function(x) {
  
  if(unique(nd$trip) == "Schoodic Peninsula") {
    
    dat <- nd %>% 
      filter(num.observers < 100) %>% 
      select(common.name:location.id, date, num.observers)
  
  } else {
    
    dat <- nd %>% 
      select(common.name:location.id, date, num.observers)
    
  }
  
  
  output <- data.frame(`Number_of_species` = length(unique(dat$scientific.name)),
             `Number_of_checklists` = length(unique(dat$submission.id)),
             `Number_of_years_run` = length(unique(year(dat$date))),
             `Average_number_of_participants` = round(mean(dat$num.observers), 0)) %>% 
    pivot_longer(`Number_of_species`:`Average_number_of_participants`) %>% 
    rename(Category = name)
    
  
  return(output)
}
 