#### Script for plotting HHS maps by RKM for UWW #####
#### Tulley Mackey ####

# load libraries #

library(tidyverse)
library(ggplot2)
library(readr)
library(here)
library(dplyr)
library(sf)

# upload hsi metric raw files for each species/life stage and year into one df
hhs_a = list.files("analysis/hsi_outputs/csvs", pattern = "*.csv", full.names = T) %>%
  map_df(~read_csv(.)) %>%
  rename("Reach_ID" = ID) %>%
  left_join(st_read(here("analysis/data/derived_data/hsi/rkm_poly.shp")))

geom = st_read(here("analysis/data/derived_data/hsi/geom_poly.shp"))

hhs_sum = merge(hhs_a, geom, by = "Reach_ID") %>%
  select(Reach_ID, HHS, geometry.y, scenario, year, Strm_Name.y) %>%
  rename("geometry" = geometry.y, "Watershed" = Strm_Name.y) %>%
  ggplot() +
  geom_sf(aes(fill = Watershed)) +
  geom_sf_label(aes(label = Reach_ID),
                    size = 2,
                    nudge_x = 3000,
                    nudge_y = 500) +
  scale_fill_distiller(palette = "Spectral",
                           direction = 1) +
  theme_bw() +
  labs(fill = "Mean Composite Suitability") +
  facet_grid(scenario ~ year) +
  theme(#axis.text.x = element_text(angle = -45, vjust = 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top")

