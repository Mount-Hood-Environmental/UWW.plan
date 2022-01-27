# Authors: Mike Ackerman & Kevin See
# Purpose: Prep data to be used in the UWW assessment
# Created: 10/16/2020
# Last Modified: 1/27/2022
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(sf)
library(here)
library(tidyverse)
library(magrittr)
library(ggmap)
library(nngeo)

#-----------------------------------------------------------------
# set NAS prefix, depending on operating system
if(.Platform$OS.type != 'unix') {nas_prefix = "S:"}

# set default crs
ww_crs = st_crs(4326) # WGS84

#-----------------------------------------------------------------
# read in HUC12 watershed boundaries from NAS, majority of PNW
huc12_sf = st_read(paste0(nas_prefix, "data/habitat/watershed_boundaries/WBDHU12.shp")) %>%
  st_transform(ww_crs)

#-----------------------------------------------------------------
# read in current QRF extrapolations, for entire CRB
# use Morgan Bond's spatially continuous, 200m linear network layer
# use random forest extrapolation model

# sum_juv_sf = st_read(paste0(nas_prefix, "data/qrf/extrapolations/Rch_Cap_RF_juv_summer_dash.gpkg")) %>% st_transform(ww_crs)
# win_juv_sf = st_read(paste0(nas_prefix, "data/qrf/extrapolations/Rch_Cap_RF_juv_winter.gpkg")) %>% st_transform(ww_crs)
# redd_sf = st_read(paste0(nas_prefix, "data/qrf/extrapolations/Rch_Cap_RF_redds.gpkg")) %>% st_transform(ww_crs)

# if needed, these .gpkg files can be saved locally to decrease read times, e.g.,
# on Mike's machine...
sum_juv_sf = st_read("C:/Workspace/qrf/Rch_Cap_RF_juv_summer_dash.gpkg") %>%
  st_transform(ww_crs)
win_juv_sf = st_read("C:/Workspace/qrf/Rch_Cap_RF_juv_winter.gpkg") %>%
  st_transform(ww_crs)
redd_sf = st_read("C:/Workspace/qrf/Rch_Cap_RF_redds.gpkg") %>%
  st_transform(ww_crs)
# these are too large to reasonably plot

#-----------------------------------------------------------------
# focus on Walla Walla, and compute total capacity at each reach
# note that this does NOT filter for species ranges, yet
ww_huc_sf = st_read(here("analysis/data/raw_data/watershed_boundary/UpperWWAssessmentStudyArea.shp")) %>%
  st_transform(ww_crs) %>%
  mutate(HUC8 = str_sub(HUC12, 1, 8),
         HUC10 = str_sub(HUC12, 1, 10))

# plot ww_huc_sf
ggplot(data = ww_huc_sf) +
  geom_sf()

#-----------------------------------------------------------------
# filter WW QRF extrapolations

# summer parr
ww_sum_sf = sum_juv_sf %>%
  st_intersection(ww_huc_sf %>%
                    st_union() %>%
                    nngeo::st_remove_holes()) %>%
  # calculate summer, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_m_se * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_m_se * reach_leng) %>%
  # add HUC codes
  st_join(ww_huc_sf %>%
            select(Name,
                   HUC8,
                   HUC10,
                   HUC12),
          join = st_covered_by,
          left = F)

ggplot(ww_sum_sf) + geom_sf()

# winter presmolt
ww_win_sf = win_juv_sf %>%
  st_intersection(ww_huc_sf %>%
                    st_union() %>%
                    nngeo::st_remove_holes()) %>%
  # calculate winter, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_m_se * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_m_se * reach_leng) %>%
  # add HUC codes
  st_join(ww_huc_sf %>%
            select(Name,
                   HUC8,
                   HUC10,
                   HUC12),
          join = st_covered_by,
          left = F)

ggplot(ww_win_sf) + geom_sf()

# redds
ww_redd_sf = redd_sf %>%
  st_intersection(ww_huc_sf %>%
                    st_union() %>%
                    nngeo::st_remove_holes()) %>%
  # calculate redd capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_m_se * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_m_se * reach_leng) %>%
  # add HUC codes
  st_join(ww_huc_sf %>%
            select(Name,
                   HUC8,
                   HUC10,
                   HUC12),
          join = st_covered_by,
          left = F)

ggplot(ww_redd_sf) + geom_sf()

#-----------------------------------------------------------------
# get 200m reach layer for this area
# this dataset is in the QRFcapacity repo
load("C:/Git/QRFcapacity/data/rch_200.rda")

# alternative by installing QRFcapacity R package...
# library(devtools)
# devtools::install_github("KevinSee/QRFcapacity")
# library(QRFcapacity)
# data("rch_200")

ww_rch_sf = rch_200 %>%
  filter(UniqueID %in% ww_sum_sf$UniqueID) %>%
  st_transform(ww_crs)

ggplot(ww_rch_sf) + geom_sf()

# compare with the stream network provided by CTUIR
ctuir_rch_sf = st_read(here("analysis/data/raw_data/stream_network/CTUIRStillwaterStreamNetwork.shp")) %>%
  st_transform(ww_crs)

ggplot(ctuir_rch_sf) + geom_sf()

# read in UWW base referencing geodatabase
ww_gdb_filepath = here("analysis/data/raw_data/stream_network/UWW_base_referencing.gdb")
ww_gdb_layers = ogrListLayers(ww_gdb_filepath)

# extract UWW_streams_24k layer from geodatabase
ww_base_ref_streams_24k  = readOGR(dsn = ww_gdb_filepath,
                                   layer = "UWW_streams_24k") %>%
  st_as_sf() %>%
  st_transform(ww_crs)

ggplot(ww_base_ref_streams_24k) + geom_sf()

# Thankfully, the base reference streams and our rch_200 appear to be the same linear networks!!!

#-----------------------------------------------------------------
# save data for this repository
save(ww_sum_sf,
     ww_win_sf,
     ww_redd_sf,
     ww_huc_sf,
     ww_rch_sf,
     ww_base_ref_streams_24k,
     file = "analysis/data/derived_data/qrf_extrapolations.rda")

#-----------------------------------------------------------------
# save geopackages for use in QGIS
st_write(ww_sum_sf,
         dsn = "analysis/data/derived_data/ww_juv_sum_qrf.gpkg",
         append = F)
st_write(ww_win_sf,
         dsn = "analysis/data/derived_data/ww_juv_win_qrf.gpkg",
         append = F)
st_write(ww_redd_sf,
         dsn = "analysis/data/derived_data/ww_redd_qrf.gpkg",
         append = F)
st_write(ww_rch_sf,
         dsn = "analysis/data/derived_data/ww_rch_200.gpkg",
         append = F)
st_write(ww_huc_sf,
         dsn = "analysis/data/derived_data/ww_huc_bndry.gpkg",
         append = F)
st_write(ww_base_ref_streams_24k,
         dsn = "analysis/data/derived_data/ww_base_ref_streams.gpkg",
         append = F)

#-----------------------------------------------------------------
# just a preliminary plot

# pick a river color
river_color = "lightskyblue1"

ww_sum_sf %>%
  ggplot() +
  geom_sf(data = ww_huc_sf %>%
            st_union() %>%
            nngeo::st_remove_holes(),
          fill = NA,
          color = "gray50") +
  geom_sf(color = river_color) +
  geom_sf(data = ww_sum_sf %>%
            filter(chnk), # only records in the StreamNet Chinook domain
          aes(color = chnk_per_m2),
          size = 1) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  scale_color_viridis_c(direction = -1) +
  labs(title = "Upper Walla Walla Watershed",
       color = expression(`Chinook Parr` / m^2))
