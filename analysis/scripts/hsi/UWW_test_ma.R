######################################
#
# Purpose: Script for running a single HSI analysis
# for the Upper Walla Walla watershed
#
# Created by: Tulley Mackey
#   Modified by: Mike Ackerman
#
# Created on: some date
#   Last modified: February 17, 2023
#
#####################################################################

# load libraries
library(here)
library(raster)
library(sf)
library(tidyverse)

# read in depth and velocity rasters
d_rast <- raster(here("analysis/data/raw_data/hsi_d_v_tifs/NF_Depth_2019.tif"))
v_rast <- raster(here("analysis/data/raw_data/hsi_d_v_tifs/NF_Velocity_2019.tif"))

# read in hsi curves
source(here("analysis/scripts/hsi/hsi_curves.R"))

# read in geomorphic reach polygon
reach_nf = st_read(here("analysis/data/derived_data/hsi/nf_geom_poly.shp")) %>%
  mutate(Name = Reach_ID)

# create one object for each reach
rch_names = unique(reach_nf$Name)
for(rch in rch_names) { assign(as.character(rch), filter(reach_nf, Name == rch)) }

# get the appropriate suitability curve(s)
d_curve = paste0("chnk_spw_d")
v_curve = paste0("chnk_spw_v")

# calculate suitability using those curves
d_suit = calc(d_rast, get(d_curve))
v_suit = calc(v_rast, get(v_curve))

# write rasters
writeRaster(d_suit, here("analysis/hsi_outputs/nf_chnk_spw_d_suit.tif"), overwrite = T)
writeRaster(d_suit, here("analysis/hsi_outputs/nf_chnk_spw_v_suit.tif"), overwrite = T)

# function to calculate composite suitability using geometric mean of depth and velocity suitabilities
comp_hsi <- function(r1, r2) {
  y = (sqrt(r1*r2))
  return(y)
}

# calculate geometric mean and create a new raster
comp_suit <- overlay(d_suit, v_suit, fun = comp_hsi)
writeRaster(comp_suit, here("analysis/hsi_outputs/nf_chnk_spw_comp_suit.tif"), overwrite = T)

# calculate metrics at geomorphic reach scale including wetted area, WUA, and HHS
# extract raster values using polygons
for(rch in rch_names) {
  sf_tmp = raster::extract(comp_suit,
                           get(as.character(rch)),
                           fun = NULL,
                           df = TRUE,
                           na.rm = TRUE) %>%
    drop_na() %>%
    mutate(ID = replace(ID, ID == 1, as.character(rch)))
  assign(paste0("hsi_extract_", rch), sf_tmp)
  rm(sf_tmp)
}

# merge back together
hsi_merge = bind_rows(lapply(ls(pattern = "^hsi_extract"), function(x) get(x)))
names(hsi_merge) = c("ID", "value")

# get pixel area. UWW seems to be 1m x 1m
pix_area = prod(res(comp_suit)) #

# calculate HSI metrics
hsi_mets = hsi_merge %>%
  mutate(pix_area = pix_area) %>%
  group_by(ID) %>%
  summarise(area_m2 = sum(pix_area),
            WUA = sum(value),
            HHS = WUA/area_m2)

# export HSI metrics output to csv
write_csv(hsi_mets, here("analysis/hsi_outputs/nf_spw_hsi_output.csv"))
