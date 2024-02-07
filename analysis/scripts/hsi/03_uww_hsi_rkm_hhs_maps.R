#### Script for plotting HHS maps by RKM for UWW #####
#### Tulley Mackey ####

# load libraries #

library(tidyverse)
library(ggplot2)
library(readr)
library(here)
library(dplyr)
library(sf)

###### SUMMER FLOW #####

# read rkm shapefile, csv output, merge and plot #

rkm = (st_read(here("analysis/data/derived_data/hsi/rkm_poly.shp"))) %>%
  select(Reach_ID, Strm_Name, geometry) %>%
  rename(watershed = Strm_Name)

geo = (st_read(here("analysis/data/derived_data/hsi/geom_buffer.shp"))) %>%
  select(Reach, geometry)

geo_ms = geo %>%
  filter(str_starts(Reach, "MS"))

geo_nf = geo %>%
  filter(str_starts(Reach, "NF"))

geo_sf = geo %>%
  filter(str_starts(Reach, "SF"))

hhs = list.files("analysis/hsi_outputs/csvs", pattern = "*.csv", full.names = T)%>%
  map_df(~read_csv(.)) %>%
  rename(Reach_ID = ID) %>%
  filter(str_starts(Reach_ID, "RKM"))

hhs_ms = hhs %>%
  filter(str_ends(Reach_ID, "MS"))

hhs_nf = hhs %>%
  filter(str_ends(Reach_ID, "NF"))

hhs_sf = hhs %>%
  filter(str_ends(Reach_ID, "SF"))

hhs_fork = bind_rows(hhs_nf, hhs_sf)

scenario_labs = c("Chinook Juvenile", "Chinook Spawning", "Steelhead Juvenile")
names(scenario_labs) = c("chnk_juv", "chnk_spw", "sthd_juv")

# HHS maps

hhs_rkm_ms = merge(rkm, hhs_ms, by = "Reach_ID") %>%
ggplot() +
geom_sf(aes(fill = HHS)) +
scale_fill_distiller(palette = "Spectral",
                      direction = 1,
                      name = "Hydraulic Habitat Suitability (HHS)") +
  geom_sf(data = geo_ms, color = 'grey50',
          fill = NA) +
  geom_sf_label(data = geo_ms, aes(label = Reach),
                color = 'black',
                size = 2,
                nudge_x = 2500,
                nudge_y = 1000) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top") +
  facet_grid(year ~ scenario, labeller = labeller(scenario = scenario_labs))
hhs_rkm_ms

hhs_rkm_fork = merge(rkm, hhs_fork, by = "Reach_ID") %>%
  ggplot() +
  geom_sf(aes(fill = HHS)) +
  scale_fill_distiller(palette = "Spectral",
                       direction = 1,
                       name = "Hydraulic Habitat Suitability (HHS)") +
  geom_sf(data = geo_fork, color = 'grey50',
          fill = NA) +
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
hhs_rkm_fork


# Depth maps
depth_rkm_ms = merge(rkm, hhs_ms, by = "Reach_ID") %>%
  ggplot() +
  geom_sf(aes(fill = d_HSI)) +
  scale_fill_distiller(palette = "Spectral",
                       direction = 1,
                       name = "Habitat Suitability by Depth") +
  geom_sf(data = geo_ms, color = 'grey50',
          fill = NA) +
  geom_sf_label(data = geo_ms, aes(label = Reach),
                color = 'black',
                size = 2,
                nudge_x = -3500,
                nudge_y = -1500) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top") +
  facet_grid(year ~ scenario, labeller = labeller(scenario = scenario_labs))
depth_rkm_ms

depth_rkm_fork = merge(rkm, hhs_fork, by = "Reach_ID") %>%
  ggplot() +
  geom_sf(aes(fill = d_HSI)) +
  scale_fill_distiller(palette = "Spectral",
                       direction = 1,
                       name = "Hydraulic Habitat Suitability (HHS)") +
  geom_sf(data = geo_fork, color = 'grey50',
          fill = NA) +
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
depth_rkm_fork

# Velocity maps
vel_rkm_ms = merge(rkm, hhs_ms, by = "Reach_ID") %>%
  ggplot() +
  geom_sf(aes(fill = v_HSI)) +
  scale_fill_distiller(palette = "Spectral",
                       direction = 1,
                       name = "Habitat Suitability by Velocity (depth avg)") +
  geom_sf(data = geo_ms, color = 'grey50',
          fill = NA) +
  geom_sf_label(data = geo_ms, aes(label = Reach),
                color = 'black',
                size = 2,
                nudge_x = -3500,
                nudge_y = -1500) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top") +
  facet_grid(year ~ scenario, labeller = labeller(scenario = scenario_labs))
vel_rkm_ms

vel_rkm_fork = merge(rkm, hhs_fork, by = "Reach_ID") %>%
  ggplot() +
  geom_sf(aes(fill = v_HSI)) +
  scale_fill_distiller(palette = "Spectral",
                       direction = 1,
                       name = "Hydraulic Habitat Suitability (HHS)") +
  geom_sf(data = geo_fork, color = 'grey50',
          fill = NA) +
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
vel_rkm_fork


##### WINTER FLOW #######


rkm = (st_read(here("analysis/data/derived_data/hsi/rkm_poly.shp"))) %>%
  select(Reach_ID, Strm_Name, geometry) %>%
  rename(watershed = Strm_Name)

geo = (st_read(here("analysis/data/derived_data/hsi/geom_buffer.shp"))) %>%
  select(Reach, geometry)

geo_ms = geo %>%
  filter(str_starts(Reach, "MS"))

geo_nf = geo %>%
  filter(str_starts(Reach, "NF"))

geo_sf = geo %>%
  filter(str_starts(Reach, "SF"))

geo_fork = bind_rows(geo_nf,geo_sf)

hhs = list.files("analysis/hsi_outputs/csvs", pattern = "*.csv", full.names = T)%>%
  map_df(~read_csv(.)) %>%
  rename(Reach_ID = ID) %>%
  filter(str_starts(Reach_ID, "RKM"))

hhs_ms = hhs %>%
  filter(str_ends(Reach_ID, "MS"))

hhs_nf = hhs %>%
  filter(str_ends(Reach_ID, "NF"))

hhs_sf = hhs %>%
  filter(str_ends(Reach_ID, "SF"))

hhs_fork = bind_rows(hhs_nf, hhs_sf)

scenario_labs = c("Chinook Juvenile Winter")    #, "Chinook Spawning", "Steelhead Juvenile")
names(scenario_labs) = c("chnk_win")              #, "chnk_spw", "sthd_juv")

# HHS maps

hhs_rkm_ms = merge(rkm, hhs_ms, by = "Reach_ID") %>%
  ggplot() +
  geom_sf(aes(fill = HHS)) +
  scale_fill_distiller(palette = "Spectral",
                       direction = 1,
                       name = "Hydraulic Habitat Suitability (HHS)") +
  geom_sf(data = geo_ms, color = 'grey50',
          fill = NA) +
  geom_sf_label(data = geo_ms, aes(label = Reach),
                color = 'black',
                size = 2,
                nudge_x = 2500,
                nudge_y = 1000) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top") +
  facet_grid(year ~ scenario, labeller = labeller(scenario = scenario_labs))
hhs_rkm_ms

hhs_rkm_fork = merge(rkm, hhs_fork, by = "Reach_ID") %>%
  ggplot() +
  geom_sf(aes(fill = HHS)) +
  scale_fill_distiller(palette = "Spectral",
                       direction = 1,
                       name = "Hydraulic Habitat Suitability (HHS)") +
  geom_sf(data = geo_fork, color = 'grey50',
          fill = NA) +
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
hhs_rkm_fork


# Depth maps
depth_rkm_ms = merge(rkm, hhs_ms, by = "Reach_ID") %>%
  ggplot() +
  geom_sf(aes(fill = d_HSI)) +
  scale_fill_distiller(palette = "Spectral",
                       direction = 1,
                       name = "Habitat Suitability by Depth") +
  geom_sf(data = geo_ms, color = 'grey50',
          fill = NA) +
  geom_sf_label(data = geo_ms, aes(label = Reach),
                color = 'black',
                size = 2,
                nudge_x = -3500,
                nudge_y = -1500) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top") +
  facet_grid(year ~ scenario, labeller = labeller(scenario = scenario_labs))
depth_rkm_ms

depth_rkm_fork = merge(rkm, hhs_fork, by = "Reach_ID") %>%
  ggplot() +
  geom_sf(aes(fill = d_HSI)) +
  scale_fill_distiller(palette = "Spectral",
                       direction = 1,
                       name = "Hydraulic Habitat Suitability (HHS)") +
  geom_sf(data = geo_fork, color = 'grey50',
          fill = NA) +
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
depth_rkm_fork

# Velocity maps
vel_rkm_ms = merge(rkm, hhs_ms, by = "Reach_ID") %>%
  ggplot() +
  geom_sf(aes(fill = v_HSI)) +
  scale_fill_distiller(palette = "Spectral",
                       direction = 1,
                       name = "Habitat Suitability by Velocity (depth avg)") +
  geom_sf(data = geo_ms, color = 'grey50',
          fill = NA) +
  geom_sf_label(data = geo_ms, aes(label = Reach),
                color = 'black',
                size = 2,
                nudge_x = -3500,
                nudge_y = -1500) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top") +
  facet_grid(year ~ scenario, labeller = labeller(scenario = scenario_labs))
vel_rkm_ms

vel_rkm_fork = merge(rkm, hhs_fork, by = "Reach_ID") %>%
  ggplot() +
  geom_sf(aes(fill = v_HSI)) +
  scale_fill_distiller(palette = "Spectral",
                       direction = 1,
                       name = "Hydraulic Habitat Suitability (HHS)") +
  geom_sf(data = geo_fork, color = 'grey50',
          fill = NA) +
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
vel_rkm_fork




