######################################
#
# Purpose: Script for calculating WUA and HHS using depth and velocity
#   rasters for the Upper Walla Walla watershed. Loops over all possible
#   scenarios/runs
#
# Created by: Tulley Mackey
#   Modified by: Mike Ackerman
#
# Created on: February 20, 2023
#   Last modified: February 17, 2023
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
wtsd = "nf"
scn  = "chnk_spw"
yr   = "2019"
scl  = "geo"

# to loop over scenarios
watersheds = c("nf", "sf") #c("nf", "sf", "ww")
scenarios  = c("chnk_spw", "chnk_juv", "sthd_juv")
years      = c("2019", "2021")
scales     = c("geo", "rkm")

for(wtsd in watersheds) {
  for(scn in scenarios) {
    for(yr in years) {
      for(scl in scales) {

      ## read in appropriate depth and velocity rasters
      if(wtsd == "nf" & yr == "2019") {
        d_rast = raster(here("analysis/data/raw_data/hsi_d_v_tifs/NF_Depth_2019.tif"))
        v_rast <- raster(here("analysis/data/raw_data/hsi_d_v_tifs/NF_Velocity_2019.tif"))
      }
      if(wtsd == "nf" & yr == "2021") {
        d_rast = raster(here("analysis/data/raw_data/hsi_d_v_tifs/NF_Depth_2021.tif"))
        v_rast <- raster(here("analysis/data/raw_data/hsi_d_v_tifs/NF_Velocity_2021.tif"))
      }
      if(wtsd == "sf" & yr == "2019") {
        d_rast = raster(here("analysis/data/raw_data/hsi_d_v_tifs/SF_Depth_2019.tif"))
        v_rast <- raster(here("analysis/data/raw_data/hsi_d_v_tifs/SF_Velocity_2019.tif"))
      }
      if(wtsd == "sf" & yr == "2021") {
        d_rast = raster(here("analysis/data/raw_data/hsi_d_v_tifs/SF_Depth_2021.tif"))
        v_rast <- raster(here("analysis/data/raw_data/hsi_d_v_tifs/SF_Velocity_2021.tif"))
      }
      # these might need to be modified after we get the mainstem rasters
      # if(wtsd == "ww" & yr == "2019") {
      #   d_rast = raster(here("analysis/data/raw_data/hsi_d_v_tifs/WW_Depth_2019.tif"))
      #   v_rast <- raster(here("analysis/data/raw_data/hsi_d_v_tifs/WW_Velocity_2019.tif"))
      # }
      # if(wtsd == "ww" & yr == "2021") {
      #   d_rast = raster(here("analysis/data/raw_data/hsi_d_v_tifs/WW_Depth_2021.tif"))
      #   v_rast <- raster(here("analysis/data/raw_data/hsi_d_v_tifs/WW_Velocity_2021.tif"))
      # }

      ## read in appropriate geomorphic reach and rkm polygons for summarizing results
      if(wtsd == "nf" & scl == "geo") { poly =  st_read(here("analysis/data/derived_data/hsi/nf_geom_poly.shp")) %>% mutate(name = Reach_ID) }
      if(wtsd == "nf" & scl == "rkm") { poly =  st_read(here("analysis/data/derived_data/hsi/nf_rkm_poly.shp")) %>% mutate(name = Reach_ID) }
      if(wtsd == "sf" & scl == "geo") { poly =  st_read(here("analysis/data/derived_data/hsi/sf_geom_poly.shp")) %>% mutate(name = Reach_ID) }
      if(wtsd == "sf" & scl == "rkm") { poly =  st_read(here("analysis/data/derived_data/hsi/sf_rkm_poly.shp")) %>% mutate(name = Reach_ID) }
      #if(wtsd == "ww" & scl == "geo") { poly =  st_read(here("analysis/data/derived_data/hsi/ww_geom_poly.shp")) %>% mutate(name = Reach_ID) }
      #if(wtsd == "ww" & scl == "rkm") { poly =  st_read(here("analysis/data/derived_data/hsi/ww_rkm_poly.shp")) %>% mutate(name = Reach_ID) }

      # create one object for each polygon
      rch_names = unique(poly$name)
      for(rch in rch_names) {
        assign(as.character(rch), filter(poly, name == rch))
      }

      # read in suitability curve functions
      source(here("analysis/scripts/hsi/hsi_curves.R"))

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
      writeRaster(d_suit, paste0(here(), "/analysis/hsi_outputs/", wtsd, "_", scn, "_", yr, "_d_suit.tif"), overwrite = T)
      writeRaster(v_suit, paste0(here(), "/analysis/hsi_outputs/", wtsd, "_", scn, "_", yr, "_v_suit.tif"), overwrite = T)

      # function to calculate composite suitability using geometric mean of depth and velocity
      comp_hsi <- function(r1, r2) {
        y = (sqrt(r1*r2))
        return(y)
      }

      # calculate geometric mean of d and v suitabilities and write composite raster
      comp_suit <- overlay(d_suit, v_suit, fun = comp_hsi)
      writeRaster(comp_suit, paste0(here(), "/analysis/hsi_outputs/", wtsd, "_", scn, "_", yr, "_comp_suit.tif"), overwrite = T)

      # calculate metrics for each polygon including wettted area, WUA, and HHS
      # extract raster values within each polygon

      # depth
      for(rch in rch_names) {
        sf_tmp = raster::extract(d_suit,
                                 get(as.character(rch)),
                                 fun = NULL,
                                 df = TRUE,
                                 na.rm = TRUE) %>%
          drop_na() %>%
          mutate(ID = replace(ID, ID == 1, as.character(rch)))
        assign(paste0("hsi_d_extract_", rch), sf_tmp)
        rm(sf_tmp)
      }
      # velocity
      for(rch in rch_names) {
        sf_tmp = raster::extract(v_suit,
                                 get(as.character(rch)),
                                 fun = NULL,
                                 df = TRUE,
                                 na.rm = TRUE) %>%
          drop_na() %>%
          mutate(ID = replace(ID, ID == 1, as.character(rch)))
        assign(paste0("hsi_v_extract_", rch), sf_tmp)
        rm(sf_tmp)
      }
      # composite
      for(rch in rch_names) {
        sf_tmp = raster::extract(comp_suit,
                                 get(as.character(rch)),
                                 fun = NULL,
                                 df = TRUE,
                                 na.rm = TRUE) %>%
          drop_na() %>%
          mutate(ID = replace(ID, ID == 1, as.character(rch)))
        assign(paste0("hsi_comp_extract_", rch), sf_tmp)
        rm(sf_tmp)
      }

      # merge "reach" HSI's back together
      hsi_d_merge    = bind_rows(lapply(ls(pattern = "^hsi_d_extract"), function(x) get(x) ))
      hsi_v_merge    = bind_rows(lapply(ls(pattern = "^hsi_v_extract"), function(x) get(x) ))
      hsi_comp_merge = bind_rows(lapply(ls(pattern = "^hsi_comp_extract"), function(x) get(x) ))
      names(hsi_d_merge) = c("ID", "value")
      names(hsi_v_merge) = c("ID", "value")
      names(hsi_comp_merge) = c("ID", "value")

      # get pixel area, UWW seems to be 1m x 1m
      pix_area = prod(res(comp_suit))

      # calculate HSI metrics for the run
      hsi_mets = hsi_comp_merge %>%
        mutate(pix_area = pix_area) %>%
        group_by(ID) %>%
        summarise(area_m2 = sum(pix_area),
                  WUA = sum(value),
                  HHS = WUA/area_m2) %>%
        left_join(hsi_d_merge %>%
                 mutate(pix_area = pix_area) %>%
                 group_by(ID) %>%
                 summarise(d_HSI = sum(value) / sum(pix_area))) %>%
        left_join(hsi_v_merge %>%
                    mutate(pix_area = pix_area) %>%
                    group_by(ID) %>%
                    summarise(v_HSI = sum(value) / sum(pix_area))) %>%
        mutate(watershed = wtsd,
               scenario = scn,
               year = yr,
               scale = scl)

      # write out HSI results to .csv
      write_csv(hsi_mets, paste0(here(), "/analysis/hsi_outputs/", wtsd, "_", scn, "_", yr, "_", scl, "_hsi_mets.csv"))

      } # end scale loop
    } # end year loop
  } # end scenarios loop
} # end watersheds loop
