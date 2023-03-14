#### Script for plotting HHS maps by RKM for UWW #####
#### Tulley Mackey ####

# load libraries #

library(tidyverse)
library(ggplot2)
library(readr)
library(here)
library(dplyr)
library(sf)

# read rkm shapefile, csv output, merge and plot #

rkm = (st_read(here("analysis/data/derived_data/hsi/rkm_poly.shp"))) %>%
  select(Reach_ID, Strm_Name, geometry) %>%
  rename(watershed = Strm_Name)

geo = (st_read(here("analysis/data/derived_data/hsi/geom_poly.shp"))) %>%
  select(Reach, geometry) %>%

geo_ww = geo %>%
  filter(str_starts(Reach, "WW"))

geo_nf = geo %>%
  filter(str_starts(Reach, "NF"))

geo_sf = geo %>%
  filter(str_starts(Reach, "SF"))

hhs = list.files("analysis/hsi_outputs/csvs", pattern = "*.csv", full.names = T) %>%
  map_df(~read_csv(.)) %>%
  select(ID, HHS, scenario, year) %>%
  rename(Reach_ID = ID) %>%
  filter(str_starts(Reach_ID, "RKM"))

scenario_labs = c("Chinook Juvenile", "Chinook Spawning", "Steelhead Juvenile")
names(scenario_labs) = c("chnk_juv", "chnk_spw", "sthd_juv")

hhs_rkm = merge(rkm, hhs, by = "Reach_ID") %>%
  ggplot() +
  geom_sf(aes(fill = HHS)) +
  scale_fill_distiller(palette = "Spectral",
                        direction = 1,
                        name = "Hydraulic Habitat Suitability (HHS)") +
  geom_sf_label(data = geo_ww, aes(label = Reach),
                color = 'black',
                size = 2,
                nudge_x = -3500,
                nudge_y = -1500) +
  geom_sf_label(data = geo_nf, aes(label = Reach),
                color = 'black',
                size = 2,
                nudge_x = 50,
                nudge_y = 2000) +
  geom_sf_label(data = geo_sf, aes(label = Reach),
                color = 'black',
                size = 2,
                nudge_x = 50,
                nudge_y = -3000) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top") +
  facet_grid(year ~ scenario, labeller = labeller(scenario = scenario_labs))
hhs_rkm

