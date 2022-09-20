## Schoodic Institute at Acadia National Park
## Acadia Birding Festival

# Script to manipulate the raw data

#-------------------------------------------#
####           Load packages             ####
#-------------------------------------------#

library(tidyverse)
library(scales)



#-------------------------------------------#
####             Make plots              ####
#-------------------------------------------#

# Format data
plotdat <- tibble(read.csv("outputs/species_per_year.csv")) %>% 
  mutate(year = as.character(year)) %>% 
  filter(year > 2009)

# Create plot
plotdat %>% 
  ggplot(aes(year, total, fill = total)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = total), vjust = -1, size = 3.5) +
  scale_y_continuous(expand = c(0,0), limits = c(0,220)) +
  scale_fill_stepsn(colors = c("white", "#E0FFFF", "#061E7E"),
                    values = rescale(c(30, 140, max(plotdat$total))),
                    n.breaks = 60) +
  labs(x = "Year", y = "Total species") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12))

# Save plot
ggsave("outputs/species_year_plot.png", width = 7, height = 4.5)




