######################################
#
# Purpose: Script for calculating WUA and HHS using depth and velocity
#   rasters for the Upper Walla Walla watershed. Loops over all possible
#   scenarios/runs
#
# Created by: Tulley Mackey
#   Modified by: Bryce Oldemeyer
#
# Created on: February 20, 2023
#   Last modified: Jan 29, 2024
#
#####################################################################

# clear environment
rm(list = ls())

# load libraries
library(here)
library(raster)
library(sf)
library(tidyverse)

# set some arguments
# a single run:
# scn  = "chnk_spw"
# yr   = "2019"

# to loop over scenarios
scenarios  = c("chnk_spw", "chnk_juv", "sthd_juv")
years      = c("2019", "2021")

# read in suitability curve functions
source(here("analysis/scripts/hsi/hsi_curves_uww.R"))

# read in geomorphic reach and rkm polygons for summarizing results
geo_poly = st_read(here("analysis/data/derived_data/hsi/geom_poly.shp"))
rkm_poly = st_read(here("analysis/data/derived_data/hsi/rkm_poly.shp"))

# create one object for each reach within each polygon
geo_names = unique(geo_poly$Reach_ID)
for(geo in geo_names) {
  assign(as.character(geo), filter(geo_poly, Reach_ID == geo))
}

rkm_names = unique(rkm_poly$Reach_ID)
for(rkm in rkm_names) {
  assign(as.character(rkm), filter(rkm_poly, Reach_ID == rkm))
}

for(scn in scenarios) {
  for(yr in years) {

    # read in appropriate depth and velocity rasters
    if(yr == "2019") {
      d_rast = raster(here("analysis/data/raw_data/hsi_d_v_tifs/Depth_2019.tif"))
      v_rast = raster(here("analysis/data/raw_data/hsi_d_v_tifs/Velocity_2019.tif"))
    }
    if(yr == "2021") {
      d_rast = raster(here("analysis/data/raw_data/hsi_d_v_tifs/Depth_2021.tif"))
      v_rast = raster(here("analysis/data/raw_data/hsi_d_v_tifs/Velocity_2021.tif"))
    }

    # grab the appropriate suitability curves
    if(scn == "chnk_spw") {
      d_curve = get("chnk_spw_d")
      v_curve = get("chnk_spw_v")
    }
    if(scn == "chnk_juv") {
      d_curve = get("chnk_juv_d")
      v_curve = get("chnk_juv_v")
    }
    if(scn == "sthd_juv") {
      d_curve = get("sthd_juv_d")
      v_curve = get("sthd_juv_v")
    }

    # calculate depth and velocity suitabilities
    d_suit = calc(d_rast, d_curve)
    v_suit = calc(v_rast, v_curve)

    # write out depth and velocity suitability rasters
    writeRaster(d_suit, paste0(here(), "/analysis/hsi_outputs/rasters/", scn, "_", yr, "_d_suit.tif"), overwrite = T)
    writeRaster(v_suit, paste0(here(), "/analysis/hsi_outputs/rasters/", scn, "_", yr, "_v_suit.tif"), overwrite = T)

    # function to calculate composite suitability using geometric mean of depth and velocity
    comp_hsi <- function(r1, r2) {
      y = (sqrt(r1*r2))
      return(y)
    }

    # calculate geometric mean of d and v suitabilities and write composite raster
    comp_suit <- overlay(d_suit, v_suit, fun = comp_hsi)
    writeRaster(comp_suit, paste0(here(), "/analysis/hsi_outputs/rasters/", scn, "_", yr, "_comp_suit.tif"), overwrite = T)

    # calculate metrics for each polygon including wetted area, WUA, and HHS; extract raster values within each polygon
    # first, geomorphic reaches
    for(geo in geo_names) {
      sf_tmp = raster::extract(d_suit,
                               get(as.character(geo)),
                               fun = NULL,
                               df = TRUE,
                               na.rm = TRUE) %>%
        drop_na() %>%
        mutate(ID = replace(ID, ID == 1, as.character(geo)))
      assign(paste0("geo_dsi_", geo), sf_tmp)
      rm(sf_tmp)
    }
    for(geo in geo_names) {
      sf_tmp = raster::extract(v_suit,
                               get(as.character(geo)),
                               fun = NULL,
                               df = TRUE,
                               na.rm = TRUE) %>%
        drop_na() %>%
        mutate(ID = replace(ID, ID == 1, as.character(geo)))
      assign(paste0("geo_vsi_", geo), sf_tmp)
      rm(sf_tmp)
    }
    for(geo in geo_names) {
      sf_tmp = raster::extract(comp_suit,
                               get(as.character(geo)),
                               fun = NULL,
                               df = TRUE,
                               na.rm = TRUE) %>%
        drop_na() %>%
        mutate(ID = replace(ID, ID == 1, as.character(geo)))
      assign(paste0("geo_csi_", geo), sf_tmp)
      rm(sf_tmp)
    }

    # merge geomorphic reach HSI's back together
    geo_dsi = bind_rows(lapply(ls(pattern = "^geo_dsi_"), function(x) get(x) )) %>%
      rename("value"  = "layer") %>%
      mutate("metric" = "depth")

    geo_vsi = bind_rows(lapply(ls(pattern = "^geo_vsi_"), function(x) get(x) )) %>%
      rename("value"  = "layer") %>%
      mutate("metric" = "velocity")

    geo_csi = bind_rows(lapply(ls(pattern = "^geo_csi_"), function(x) get(x) )) %>%
      rename("value"  = "layer") %>%
      mutate("metric" = "composite")

    geo_values = bind_rows(geo_dsi, geo_vsi, geo_csi) %>%
      mutate("scenario" = scn) %>%
      mutate("year" = yr)

    # write raw geomorphic reach results
    write_rds(geo_values,
              paste0(here(), "/analysis/hsi_outputs/raw_results/", scn, "_", yr, "_geo.rda"))

    # now, river kilometers
    for(rkm in rkm_names) {
      sf_tmp = raster::extract(d_suit,
                               get(as.character(rkm)),
                               fun = NULL,
                               df = TRUE,
                               na.rm = TRUE) %>%
        drop_na() %>%
        mutate(ID = replace(ID, ID == 1, as.character(rkm)))
      assign(paste0("rkm_dsi_", rkm), sf_tmp)
      rm(sf_tmp)
    }
    for(rkm in rkm_names) {
      sf_tmp = raster::extract(v_suit,
                               get(as.character(rkm)),
                               fun = NULL,
                               df = TRUE,
                               na.rm = TRUE) %>%
        drop_na() %>%
        mutate(ID = replace(ID, ID == 1, as.character(rkm)))
      assign(paste0("rkm_vsi_", rkm), sf_tmp)
      rm(sf_tmp)
    }
    for(rkm in rkm_names) {
      sf_tmp = raster::extract(comp_suit,
                               get(as.character(rkm)),
                               fun = NULL,
                               df = TRUE,
                               na.rm = TRUE) %>%
        drop_na() %>%
        mutate(ID = replace(ID, ID == 1, as.character(rkm)))
      assign(paste0("rkm_csi_", rkm), sf_tmp)
      rm(sf_tmp)
    }

    # merge river kilometer HSI's back together
    rkm_dsi = bind_rows(lapply(ls(pattern = "^rkm_dsi_"), function(x) get(x) )) %>%
      rename("value"  = "layer") %>%
      mutate("metric" = "depth")

    rkm_vsi = bind_rows(lapply(ls(pattern = "^rkm_vsi_"), function(x) get(x) )) %>%
      rename("value"  = "layer") %>%
      mutate("metric" = "velocity")

    rkm_csi = bind_rows(lapply(ls(pattern = "^rkm_csi_"), function(x) get(x) )) %>%
      rename("value"  = "layer") %>%
      mutate("metric" = "composite")

    rkm_values = bind_rows(rkm_dsi, rkm_vsi, rkm_csi) %>%
      mutate("scenario" = scn) %>%
      mutate("year" = yr)

    # write raw river kilometer results
    write_rds(rkm_values,
              paste0(here(), "/analysis/hsi_outputs/raw_results/", scn, "_", yr, "_rkm.rda"))

    # get pixel area, UWW seems to be 1m x 1m
    pix_area = prod(res(comp_suit))

    # calculate HSI metrics for the run (geomorphic reaches)
    geo_mets = geo_csi %>%
      mutate(pix_area = pix_area) %>%
      group_by(ID) %>%
      summarise(area_m2 = sum(pix_area),
                WUA = sum(value),
                HHS = WUA/area_m2) %>%
      left_join(geo_dsi %>%
                  mutate(pix_area = pix_area) %>%
                  group_by(ID) %>%
                  summarise(d_HSI = sum(value) / sum(pix_area))) %>%
      left_join(geo_vsi %>%
                  mutate(pix_area = pix_area) %>%
                  group_by(ID) %>%
                  summarise(v_HSI = sum(value) / sum(pix_area))) %>%
      mutate(scenario = scn,
             year = yr)

    # write out geomorphic reach results to .csv
    write_csv(geo_mets, paste0(here(), "/analysis/hsi_outputs/csvs/", scn, "_", yr, "_geo_results.csv"))

    # calculate HSI metrics for the run (river kilometers)
    rkm_mets = rkm_csi %>%
      mutate(pix_area = pix_area) %>%
      group_by(ID) %>%
      summarise(area_m2 = sum(pix_area),
                WUA = sum(value),
                HHS = WUA/area_m2) %>%
      left_join(rkm_dsi %>%
                  mutate(pix_area = pix_area) %>%
                  group_by(ID) %>%
                  summarise(d_HSI = sum(value) / sum(pix_area))) %>%
      left_join(rkm_vsi %>%
                  mutate(pix_area = pix_area) %>%
                  group_by(ID) %>%
                  summarise(v_HSI = sum(value) / sum(pix_area))) %>%
      mutate(scenario = scn,
             year = yr)

    # write out geomorphic reach results to .csv
    write_csv(rkm_mets, paste0(here(), "/analysis/hsi_outputs/csvs/", scn, "_", yr, "_rkm_results.csv"))

    # print message for each loop
    message(paste0("Run ", scn, " ", yr, " is complete."))

  } # end years loop
}   # end scenarios loop

#####################################################
#
#
# Updated flow scenario for juvenile winter rearing
#
# 50% Exceedence flow needs to correspond to juvenile winter rearing
# This will have 2019 and 2021
#
# Curves will need to be chnk_win_d, chnk_win_v, sthd_win_d & sthd_win_v
# Data are "Combined_2019lidar_Winter_..." & #Combined_2021lidar_Winter..."
#
#
#####################################################

# to loop over scenarios
scenarios  = c("chnk_spw", "chnk_juv", "sthd_juv") # Change to "chnk_win" and "sthd_win"
years      = c("2019", "2021") # Keep same

for(scn in scenarios) {
  for(yr in years) {

    # read in appropriate depth and velocity rasters
    if(yr == "2019") {
      d_rast = raster(here("analysis/data/raw_data/hsi_d_v_tifs/Depth_2019.tif"))
      v_rast = raster(here("analysis/data/raw_data/hsi_d_v_tifs/Velocity_2019.tif"))
    }
    if(yr == "2021") {
      d_rast = raster(here("analysis/data/raw_data/hsi_d_v_tifs/Depth_2021.tif"))
      v_rast = raster(here("analysis/data/raw_data/hsi_d_v_tifs/Velocity_2021.tif"))
    }

    # grab the appropriate suitability curves -- update to pull in winter curves
    if(scn == "chnk_spw") {
      d_curve = get("chnk_spw_d")
      v_curve = get("chnk_spw_v")
    }
    if(scn == "chnk_juv") {
      d_curve = get("chnk_juv_d")
      v_curve = get("chnk_juv_v")
    }
    if(scn == "sthd_juv") {
      d_curve = get("sthd_juv_d")
      v_curve = get("sthd_juv_v")
    }

    # calculate depth and velocity suitabilities
    d_suit = calc(d_rast, d_curve)
    v_suit = calc(v_rast, v_curve)

    # write out depth and velocity suitability rasters
    writeRaster(d_suit, paste0(here(), "/analysis/hsi_outputs/rasters/", scn, "_", yr, "_d_suit.tif"), overwrite = T)
    writeRaster(v_suit, paste0(here(), "/analysis/hsi_outputs/rasters/", scn, "_", yr, "_v_suit.tif"), overwrite = T)

    # function to calculate composite suitability using geometric mean of depth and velocity
    comp_hsi <- function(r1, r2) {
      y = (sqrt(r1*r2))
      return(y)
    }

    # calculate geometric mean of d and v suitabilities and write composite raster
    comp_suit <- overlay(d_suit, v_suit, fun = comp_hsi)
    writeRaster(comp_suit, paste0(here(), "/analysis/hsi_outputs/rasters/", scn, "_", yr, "_comp_suit.tif"), overwrite = T)

    # calculate metrics for each polygon including wetted area, WUA, and HHS; extract raster values within each polygon
    # first, geomorphic reaches
    for(geo in geo_names) {
      sf_tmp = raster::extract(d_suit,
                               get(as.character(geo)),
                               fun = NULL,
                               df = TRUE,
                               na.rm = TRUE) %>%
        drop_na() %>%
        mutate(ID = replace(ID, ID == 1, as.character(geo)))
      assign(paste0("geo_dsi_", geo), sf_tmp)
      rm(sf_tmp)
    }
    for(geo in geo_names) {
      sf_tmp = raster::extract(v_suit,
                               get(as.character(geo)),
                               fun = NULL,
                               df = TRUE,
                               na.rm = TRUE) %>%
        drop_na() %>%
        mutate(ID = replace(ID, ID == 1, as.character(geo)))
      assign(paste0("geo_vsi_", geo), sf_tmp)
      rm(sf_tmp)
    }
    for(geo in geo_names) {
      sf_tmp = raster::extract(comp_suit,
                               get(as.character(geo)),
                               fun = NULL,
                               df = TRUE,
                               na.rm = TRUE) %>%
        drop_na() %>%
        mutate(ID = replace(ID, ID == 1, as.character(geo)))
      assign(paste0("geo_csi_", geo), sf_tmp)
      rm(sf_tmp)
    }

    # merge geomorphic reach HSI's back together
    geo_dsi = bind_rows(lapply(ls(pattern = "^geo_dsi_"), function(x) get(x) )) %>%
      rename("value"  = "layer") %>%
      mutate("metric" = "depth")

    geo_vsi = bind_rows(lapply(ls(pattern = "^geo_vsi_"), function(x) get(x) )) %>%
      rename("value"  = "layer") %>%
      mutate("metric" = "velocity")

    geo_csi = bind_rows(lapply(ls(pattern = "^geo_csi_"), function(x) get(x) )) %>%
      rename("value"  = "layer") %>%
      mutate("metric" = "composite")

    geo_values = bind_rows(geo_dsi, geo_vsi, geo_csi) %>%
      mutate("scenario" = scn) %>%
      mutate("year" = yr)

    # write raw geomorphic reach results
    write_rds(geo_values,
              paste0(here(), "/analysis/hsi_outputs/raw_results/", scn, "_", yr, "_geo.rda"))

    # now, river kilometers
    for(rkm in rkm_names) {
      sf_tmp = raster::extract(d_suit,
                               get(as.character(rkm)),
                               fun = NULL,
                               df = TRUE,
                               na.rm = TRUE) %>%
        drop_na() %>%
        mutate(ID = replace(ID, ID == 1, as.character(rkm)))
      assign(paste0("rkm_dsi_", rkm), sf_tmp)
      rm(sf_tmp)
    }
    for(rkm in rkm_names) {
      sf_tmp = raster::extract(v_suit,
                               get(as.character(rkm)),
                               fun = NULL,
                               df = TRUE,
                               na.rm = TRUE) %>%
        drop_na() %>%
        mutate(ID = replace(ID, ID == 1, as.character(rkm)))
      assign(paste0("rkm_vsi_", rkm), sf_tmp)
      rm(sf_tmp)
    }
    for(rkm in rkm_names) {
      sf_tmp = raster::extract(comp_suit,
                               get(as.character(rkm)),
                               fun = NULL,
                               df = TRUE,
                               na.rm = TRUE) %>%
        drop_na() %>%
        mutate(ID = replace(ID, ID == 1, as.character(rkm)))
      assign(paste0("rkm_csi_", rkm), sf_tmp)
      rm(sf_tmp)
    }

    # merge river kilometer HSI's back together
    rkm_dsi = bind_rows(lapply(ls(pattern = "^rkm_dsi_"), function(x) get(x) )) %>%
      rename("value"  = "layer") %>%
      mutate("metric" = "depth")

    rkm_vsi = bind_rows(lapply(ls(pattern = "^rkm_vsi_"), function(x) get(x) )) %>%
      rename("value"  = "layer") %>%
      mutate("metric" = "velocity")

    rkm_csi = bind_rows(lapply(ls(pattern = "^rkm_csi_"), function(x) get(x) )) %>%
      rename("value"  = "layer") %>%
      mutate("metric" = "composite")

    rkm_values = bind_rows(rkm_dsi, rkm_vsi, rkm_csi) %>%
      mutate("scenario" = scn) %>%
      mutate("year" = yr)

    # write raw river kilometer results
    write_rds(rkm_values,
              paste0(here(), "/analysis/hsi_outputs/raw_results/", scn, "_", yr, "_rkm.rda"))

    # get pixel area, UWW seems to be 1m x 1m
    pix_area = prod(res(comp_suit))

    # calculate HSI metrics for the run (geomorphic reaches)
    geo_mets = geo_csi %>%
      mutate(pix_area = pix_area) %>%
      group_by(ID) %>%
      summarise(area_m2 = sum(pix_area),
                WUA = sum(value),
                HHS = WUA/area_m2) %>%
      left_join(geo_dsi %>%
                  mutate(pix_area = pix_area) %>%
                  group_by(ID) %>%
                  summarise(d_HSI = sum(value) / sum(pix_area))) %>%
      left_join(geo_vsi %>%
                  mutate(pix_area = pix_area) %>%
                  group_by(ID) %>%
                  summarise(v_HSI = sum(value) / sum(pix_area))) %>%
      mutate(scenario = scn,
             year = yr)

    # write out geomorphic reach results to .csv
    write_csv(geo_mets, paste0(here(), "/analysis/hsi_outputs/csvs/", scn, "_", yr, "_geo_results.csv"))

    # calculate HSI metrics for the run (river kilometers)
    rkm_mets = rkm_csi %>%
      mutate(pix_area = pix_area) %>%
      group_by(ID) %>%
      summarise(area_m2 = sum(pix_area),
                WUA = sum(value),
                HHS = WUA/area_m2) %>%
      left_join(rkm_dsi %>%
                  mutate(pix_area = pix_area) %>%
                  group_by(ID) %>%
                  summarise(d_HSI = sum(value) / sum(pix_area))) %>%
      left_join(rkm_vsi %>%
                  mutate(pix_area = pix_area) %>%
                  group_by(ID) %>%
                  summarise(v_HSI = sum(value) / sum(pix_area))) %>%
      mutate(scenario = scn,
             year = yr)

    # write out geomorphic reach results to .csv
    write_csv(rkm_mets, paste0(here(), "/analysis/hsi_outputs/csvs/", scn, "_", yr, "_rkm_results.csv"))

    # print message for each loop
    message(paste0("Run ", scn, " ", yr, " is complete."))

  } # end years loop
}   # end scenarios loop

# END SCRIPT

#####################################################
#
#
# Updated flow scenario for steelhead spawning
#
# Spring flow
#
# Curves will need to be sthd_spw_d, sthd_spw_v
# Data are "Combined_2019lidar_Spring_..." & #Combined_2021lidar_Spring..."
#
#
#####################################################


