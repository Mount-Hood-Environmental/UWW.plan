##############
# script for concatenating hsi output metrics into a df
#############

rm(list = ls())

# load library
library(tidyverse)
library(here)

# upload hsi metric .csv outputs for each watershed, species, life stage and year into one df
hsi_mets_all <-
  list.files(path = "C:/GitHub/UWW.plan/analysis/hsi_outputs", pattern = "*.csv") %>%
  map_df(~read_csv(.))

