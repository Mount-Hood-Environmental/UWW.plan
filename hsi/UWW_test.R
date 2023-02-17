library(raster)
library(rgdal)
library(sf)
library(tidyverse)
library(terra)
library(skimr)

# set working directory
setwd("C:/GitHub/UWW.plan/hsi")

# read in depth and velocity rasters
d_rast <- raster("NF_Depth_2019.tif" )
v_rast <- raster("NF_Velocity_2019.tif" )

# read in hsi curves
source("C:/GitHub/UWW.plan/hsi/hsi_curves.R")

# Read in reach polygons
reach_nf <- st_read("nf_geom_poly.shp") %>% 
  mutate(Name = Reach_ID)

# Create one object for each reach
rch_names = unique(reach_nf$Name)
for(rch in rch_names)
assign(as.character(rch), filter(reach_nf, Name == rch))

# calculate suitability with appropriate function
d_curve = paste0("chnk_spw_d")
v_curve = paste0("chnk_spw_v")
d_suit = calc(d_rast, get(d_curve))
v_suit = calc(v_rast, get(v_curve))

# write rasters
writeRaster(d_suit, paste0("C:/GitHub/UWW.plan/hsi/nf_chnk_spw_d_suit.tif"), overwrite = T)
writeRaster(v_suit, paste0("C:/GitHub/UWW.plan/hsi/nf_chnk_spw_v_suit.tif"), overwrite = T)

# Function to calculate composite suitability using geometric mean of depth and velocity
comp_hsi <- function(r1, r2) {
  y = (sqrt(r1*r2))
  return(y)
}

# Calculate geometric mean and create a new raster
comp_suit <- overlay(d_suit, v_suit, fun = comp_hsi)
writeRaster(comp_suit, paste0("C:/GitHub/UWW.plan/hsi/nf_chnk_spw_comp_suit.tif"), overwrite = T)

# Calculate metrics at reach scale wetted area, WUA, HHS
# Extract raster values at polygon 'reaches' from each object created using the reaches polygon
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

# Merge reach HSI's back together
hsi_merge = bind_rows(lapply(ls(pattern = "^hsi_extract"), function(x) get(x)))
names(hsi_merge) = c("ID", "value")

# Calculating the HSI metrics for lem the pixels are 1m x 1m = 1sq meter
pix_area = prod(res(comp_suit)) # 

hsi_mets = hsi_merge %>%
  mutate(pix_area = pix_area) %>%
  group_by(ID) %>%
  summarise(area_m2 = sum(pix_area),
            WUA = sum(value),
            HHS = WUA/area_m2)

# export metrics output to csv
write.csv(hsi_mets, "C:/GitHub/UWW.plan/hsi//nf_spw_hsi_ouput.csv", row.names = FALSE)


