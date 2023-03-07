##############
# Summarizing HSI results ##
#
# Created by: Tulley Mackey
#
# This script reads in results from calculate_hsi_mets.R
# summarizes and plots them
#############

# load library
library(tidyverse)
library(here)
library(gridExtra)
library(ggmap)
library(sf)
library(reshape2)

# upload hsi metric .csv outputs for each species, life stage and year into one df
hsi_outputs = list.files(here("analysis/hsi_outputs/csvs"), pattern = "*.csv", full.names = T) %>%
  map_df(~read_csv(.))

##################################
# Plot HHS for 1 Model at a time #
##################################
# Chinook juvenile summer rearing
chnk_juv_2019 = hsi_outputs %>%
  filter(year == "2019",
         scenario == "chnk_juv",
         str_starts(ID, "GR")) %>%
  ggplot(aes(x = ID, y = HHS, )) +
           geom_boxplot() +
           labs(x = "Geomorphic Reach",
                y = "Hydraulic Habitat Suitability",
                title = "Chinook Juvenile Rearing 2019")
chnk_juv_2019

chnk_juv = hsi_outputs %>%
  filter(year == "2019",
         scenario == "chnk_juv",
         str_starts(ID, "GR"),
         str_ends(ID, "MS")) %>%
ggplot(aes(x = ID, y = HHS, )) +
  geom_boxplot() +
  labs(x = "Geomorphic Reach",
       y = "Hydraulic Habitat Suitability",
       title = "Chinook Juvenile Rearing 2019")
chnk_juv
