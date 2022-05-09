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
library(rgdal)

#-----------------------------------------------------------------
# set NAS prefix, depending on operating system
if(.Platform$OS.type != 'unix') {nas_prefix = "S:"}

# set default crs
uww_crs = st_crs(4326) # WGS84

#-----------------------------------------------------------------
# read in HUC12 watershed boundaries from NAS, majority of PNW
huc12_sf = st_read(paste0(nas_prefix, "Public Data/data/habitat/watershed_boundaries/WBDHU12.shp")) %>%
  st_transform(uww_crs)

#-----------------------------------------------------------------
# read in current QRF extrapolations, for entire CRB
# use Morgan Bond's spatially continuous, 200m linear network layer
# use random forest extrapolation model

sum_juv_sf = st_read(paste0(nas_prefix, "Public Data/data/qrf/extrapolations/Rch_Cap_RF_juv_summer_dash.gpkg")) %>% st_transform(ww_crs)
win_juv_sf = st_read(paste0(nas_prefix, "Public Data/data/qrf/extrapolations/Rch_Cap_RF_juv_winter.gpkg")) %>% st_transform(ww_crs)
redd_sf = st_read(paste0(nas_prefix, "Public Data/data/qrf/extrapolations/Rch_Cap_RF_redds.gpkg")) %>% st_transform(ww_crs)

# if needed, these .gpkg files can be saved locally to decrease read times, e.g.,
# on Mike's machine...
# sum_juv_sf = st_read("C:/Workspace/qrf/Rch_Cap_RF_juv_summer_dash.gpkg") %>%
#   st_transform(uww_crs)
# win_juv_sf = st_read("C:/Workspace/qrf/Rch_Cap_RF_juv_winter.gpkg") %>%
#   st_transform(uww_crs)
# redd_sf = st_read("C:/Workspace/qrf/Rch_Cap_RF_redds.gpkg") %>%
#   st_transform(uww_crs)
# these are too large to reasonably plot

#-----------------------------------------------------------------
# focus on Walla Walla, and compute total capacity at each reach
# note that this does NOT filter for species ranges, yet
uww_huc_sf = st_read(here("analysis/data/raw_data/watershed_boundary/UpperWWAssessmentStudyArea.shp")) %>%
  st_transform(uww_crs) %>%
  mutate(HUC8 = str_sub(HUC12, 1, 8),
         HUC10 = str_sub(HUC12, 1, 10))

# plot ww_huc_sf
ggplot(data = uww_huc_sf) +
  geom_sf()

#-----------------------------------------------------------------
# filter WW QRF extrapolations

# summer parr
uww_sum_sf = sum_juv_sf %>%
  st_intersection(uww_huc_sf %>%
                    st_union() %>%
                    nngeo::st_remove_holes()) %>%
  # calculate summer, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_m_se * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_m_se * reach_leng) %>%
  # add HUC codes
  st_join(uww_huc_sf %>%
            select(Name,
                   HUC8,
                   HUC10,
                   HUC12),
          join = st_covered_by,
          left = F)

ggplot(uww_sum_sf) + geom_sf()

# winter presmolt
uww_win_sf = win_juv_sf %>%
  st_intersection(uww_huc_sf %>%
                    st_union() %>%
                    nngeo::st_remove_holes()) %>%
  # calculate winter, juvenile capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_m_se * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_m_se * reach_leng) %>%
  # add HUC codes
  st_join(uww_huc_sf %>%
            select(Name,
                   HUC8,
                   HUC10,
                   HUC12),
          join = st_covered_by,
          left = F)

ggplot(uww_win_sf) + geom_sf()

# redds
uww_redd_sf = redd_sf %>%
  st_intersection(uww_huc_sf %>%
                    st_union() %>%
                    nngeo::st_remove_holes()) %>%
  # calculate redd capacities for each reach
  mutate(chnk_n = chnk_per_m * reach_leng,
         chnk_n_se = chnk_per_m_se * reach_leng,
         sthd_n = sthd_per_m * reach_leng,
         sthd_n_se = sthd_per_m_se * reach_leng) %>%
  # add HUC codes
  st_join(uww_huc_sf %>%
            select(Name,
                   HUC8,
                   HUC10,
                   HUC12),
          join = st_covered_by,
          left = F)

ggplot(uww_redd_sf) + geom_sf()

#-----------------------------------------------------------------
# get 200m reach layer for this area; contains a bunch of habitat metrics for the 200m reaches
# this dataset is in the QRFcapacity repo
load("C:/Git/QRFcapacity/data/rch_200.rda")

# alternatively, by installing QRFcapacity R package...
# library(devtools)
# devtools::install_github("KevinSee/QRFcapacity")
# library(QRFcapacity)
# data("rch_200")

# trim down 200m reach layer
uww_rch_sf = rch_200 %>%
  filter(UniqueID %in% uww_sum_sf$UniqueID) %>%
  st_transform(uww_crs)

ggplot(uww_rch_sf) + geom_sf()

# compare with the stream network provided by CTUIR, this is for entire Walla Walla
ctuir_rch_sf = st_read(here("analysis/data/raw_data/stream_network/CTUIRStillwaterStreamNetwork.shp")) %>%
  st_transform(uww_crs)

ggplot(ctuir_rch_sf) + geom_sf()

# read in UWW base referencing geodatabase
uww_gdb_filepath = here("analysis/data/raw_data/stream_network/UWW_base_referencing.gdb")
uww_gdb_layers = ogrListLayers(uww_gdb_filepath)

# extract UWW_streams_24k layer from geodatabase
uww_base_ref_streams_24k  = readOGR(dsn = uww_gdb_filepath,
                                   layer = "UWW_streams_24k") %>%
  st_as_sf() %>%
  st_transform(uww_crs)

ggplot(uww_base_ref_streams_24k) + geom_sf()
# Thankfully, the base reference streams and our rch_200 appear to be the same linear networks!!!

#-----------------------------------------------------------------
# save data for this repository
save(uww_sum_sf,
     uww_win_sf,
     uww_redd_sf,
     file = "analysis/data/derived_data/uww_qrf_extrapolations.rda")

save(uww_huc_sf,
     uww_rch_sf,
     file = "analysis/data/derived_data/uww_spatial.rda")

#-----------------------------------------------------------------
# save geopackages for use in QGIS
st_write(uww_sum_sf,
         dsn = "analysis/data/derived_data/uww_juv_sum_qrf.gpkg",
         append = F)
st_write(uww_win_sf,
         dsn = "analysis/data/derived_data/uww_juv_win_qrf.gpkg",
         append = F)
st_write(uww_redd_sf,
         dsn = "analysis/data/derived_data/uww_redd_qrf.gpkg",
         append = F)
st_write(uww_rch_sf,
         dsn = "analysis/data/derived_data/ww_rch_200.gpkg",
         append = F)
st_write(uww_huc_sf,
         dsn = "analysis/data/derived_data/uww_huc_bndry.gpkg",
         append = F)
st_write(uww_base_ref_streams_24k,
         dsn = "analysis/data/derived_data/uww_base_ref_streams.gpkg",
         append = F)

#-----------------------------------------------------------------
# just a preliminary plot

# pick a river color
river_color = "lightskyblue1"

uww_sum_sf %>%
  ggplot() +
  geom_sf(data = uww_huc_sf %>%
            st_union() %>%
            nngeo::st_remove_holes(),
          fill = NA,
          color = "gray50") +
  geom_sf(color = river_color) +
  geom_sf(data = uww_sum_sf %>%
            filter(chnk), # only records in the StreamNet Chinook domain
          aes(color = chnk_per_m2),
          size = 1) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  scale_color_viridis_c(direction = -1) +
  labs(title = "Upper Walla Walla Watershed",
       color = expression(`Chinook Parr` / m^2))
