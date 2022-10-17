## Schoodic Institute at Acadia National Park
## Acadia Birding Festival

# Script to manipulate the raw data

#-------------------------------------------#
####           Load packages             ####
#-------------------------------------------#

library(tidyverse)
library(scales)
library(viridis)



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
  geom_text(aes(label = total), vjust = -1, size = 5) +
  scale_y_continuous(expand = c(0,0), limits = c(0,220)) +
  scale_fill_viridis(option = "viridis",
                     values = rescale(c(0, 140, max(plotdat$total))),
                     n.breaks = 4) +
  labs(x = "Year", y = "Total species") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(color = "black", size = 13),
        axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(color = "black", size = 15),
        plot.background = element_blank())

# Save plot
ggsave("outputs/species_year_plot.png", width = 7, height = 4.5)


species_explorer <- tibble(read.csv("outputs/all_taxa_summary.csv")) %>% 
  filter(!grepl("sp.", common.name)) %>% 
  select(-X2006, -total) %>% 
  pivot_longer(starts_with("X")) %>% 
  mutate(year = str_replace(name, "X", "")) %>%
  select(common.name, year, count = value) %>% 
  #filter(grepl("Blue", common.name))
  filter(common.name == "Bicknell's Thrush")

ggplot(species_explorer, aes(as.factor(year), common.name, fill = count)) + 
  geom_tile(colour = "gray20", size = 1.5) + 
  scale_fill_viridis(option = "viridis") +
  xlab("") + 
  ylab("") +
  scale_x_discrete(breaks = factor(2010:2022), limits = c("2010", "2011", "2012", "2013", "2014",
                                                          "2015", "2016", "2017", "2018", "2019",
                                                          "2020", "2021", "2022")) +
  ggtitle("Total individuals per year") +
  theme(plot.title = element_text(color = "white", hjust = 0, vjust = -1, size = rel(2)),
        plot.background = element_rect(fill = "gray20", color = "gray20"),
        panel.background = element_rect(fill = "gray20"),
        #panel.border = element_rect(fill = NA, color="gray20", size=0.5, linetype="solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.text = element_text(color = "white", size = rel(1.5)),
        axis.text.y  = element_text(hjust = 1),
        legend.text = element_text(color = "white", size = rel(1.3)),
        legend.background = element_rect(fill = "gray20"),
        legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        legend.title = element_blank())

ggsave("outputs/test_plot.png", width = 11, height = 3)



