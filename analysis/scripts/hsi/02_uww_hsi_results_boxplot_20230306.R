##############
# Plotting raw results ##
#
# Created by: Tulley Mackey
#
# This script reads in raw results for each metric (composite, depth, velocity) and plots them
#############

# load library
library(tidyverse)
library(here)
library(gridExtra)
library(ggmap)
library(sf)
library(reshape2)
library(readr)
library(dplyr)
library(forcats)

# upload hsi metric raw files for each species/life stage and year into one df
hsi_raw = list.files("S:/main/data/habitat/HSI/UWW_hsi_results/raw_results", pattern = "*.rda", full.names = T) %>%
  map_df(~read_rds(.)) %>%
  mutate(watershed = if_else(grepl("MS", ID), "Mainstem", ID)) %>%
  mutate(watershed = if_else(grepl("NF", ID), "North Fork", watershed)) %>%
  mutate(watershed = if_else(grepl("SF", ID), "South Fork", watershed))


##################################
# Box plot metrics (composite, depth, velocity) by geo reach and year #
##################################
# Chinook juvenile rearing 2019 #
chnk_juv_2019_geo = hsi_raw %>%
  filter(year == "2019",
         scenario == "chnk_juv",
         str_starts(ID, "GR")) %>%
  mutate(ID = factor(ID, levels = c("GR_01_MS",
                                    "GR_02_MS",
                                    "GR_03_MS",
                                    "GR_04_MS",
                                    "GR_05_MS",
                                    "GR_01_NF",
                                    "GR_02_NF",
                                    "GR_03_NF",
                                    "GR_04_NF",
                                    "GR_05_NF",
                                    "GR_01_SF",
                                    "GR_02_SF",
                                    "GR_03_SF",
                                    "GR_04_SF",
                                    "GR_05_SF"))) %>%
  ggplot(aes(x = ID, y = value, fill = watershed)) +
           geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
    theme_bw() +
           labs(x = "Geomorphic Reach",
                y = "Suitability Index",
                title = "Chinook Juvenile Rearing 2019") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
chnk_juv_2019_geo

# Chinook juvenile rearing 2021 #
chnk_juv_2021_geo = hsi_raw %>%
  filter(year == "2021",
         scenario == "chnk_juv",
         str_starts(ID, "GR")) %>%
  mutate(ID = factor(ID, levels = c("GR_01_MS",
                                    "GR_02_MS",
                                    "GR_03_MS",
                                    "GR_04_MS",
                                    "GR_05_MS",
                                    "GR_01_NF",
                                    "GR_02_NF",
                                    "GR_03_NF",
                                    "GR_04_NF",
                                    "GR_05_NF",
                                    "GR_01_SF",
                                    "GR_02_SF",
                                    "GR_03_SF",
                                    "GR_04_SF",
                                    "GR_05_SF"))) %>%
  ggplot(aes(x = ID, y = value, fill = watershed)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "Suitability Index",
       title = "Chinook Juvenile Rearing 2021") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
chnk_juv_2021_geo


# Chinook spawning 2019 #
chnk_spw_2019_geo = hsi_raw %>%
  filter(year == "2019",
         scenario == "chnk_spw",
         str_starts(ID, "GR")) %>%
  mutate(ID = factor(ID, levels = c("GR_01_MS",
                                    "GR_02_MS",
                                    "GR_03_MS",
                                    "GR_04_MS",
                                    "GR_05_MS",
                                    "GR_01_NF",
                                    "GR_02_NF",
                                    "GR_03_NF",
                                    "GR_04_NF",
                                    "GR_05_NF",
                                    "GR_01_SF",
                                    "GR_02_SF",
                                    "GR_03_SF",
                                    "GR_04_SF",
                                    "GR_05_SF"))) %>%
  ggplot(aes(x = ID, y = value, fill = watershed)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "Suitability Index",
       title = "Chinook Spawning 2019") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
chnk_spw_2019_geo

# Chinook spawning 2021 #
chnk_spw_2021_geo = hsi_raw %>%
  filter(year == "2021",
         scenario == "chnk_spw",
         str_starts(ID, "GR")) %>%
  mutate(ID = factor(ID, levels = c("GR_01_MS",
                                    "GR_02_MS",
                                    "GR_03_MS",
                                    "GR_04_MS",
                                    "GR_05_MS",
                                    "GR_01_NF",
                                    "GR_02_NF",
                                    "GR_03_NF",
                                    "GR_04_NF",
                                    "GR_05_NF",
                                    "GR_01_SF",
                                    "GR_02_SF",
                                    "GR_03_SF",
                                    "GR_04_SF",
                                    "GR_05_SF"))) %>%
  ggplot(aes(x = ID, y = value, fill = watershed)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "Suitability Index",
       title = "Chinook Spawning 2021") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
chnk_spw_2021_geo

# steelhead juvenile rearing 2019 #
sthd_juv_2019_geo = hsi_raw %>%
  filter(year == "2019",
         scenario == "sthd_juv",
         str_starts(ID, "GR")) %>%
  mutate(ID = factor(ID, levels = c("GR_01_MS",
                                    "GR_02_MS",
                                    "GR_03_MS",
                                    "GR_04_MS",
                                    "GR_05_MS",
                                    "GR_01_NF",
                                    "GR_02_NF",
                                    "GR_03_NF",
                                    "GR_04_NF",
                                    "GR_05_NF",
                                    "GR_01_SF",
                                    "GR_02_SF",
                                    "GR_03_SF",
                                    "GR_04_SF",
                                    "GR_05_SF"))) %>%
  ggplot(aes(x = ID, y = value, fill = watershed)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "Suitability Index",
       title = "Steelhead Juvenile Rearing 2019") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
sthd_juv_2019_geo

# steelhead juvenile rearing 2021 #
sthd_juv_2021_geo = hsi_raw %>%
  filter(year == "2021",
         scenario == "sthd_juv",
         str_starts(ID, "GR")) %>%
  mutate(ID = factor(ID, levels = c("GR_01_MS",
                                    "GR_02_MS",
                                    "GR_03_MS",
                                    "GR_04_MS",
                                    "GR_05_MS",
                                    "GR_01_NF",
                                    "GR_02_NF",
                                    "GR_03_NF",
                                    "GR_04_NF",
                                    "GR_05_NF",
                                    "GR_01_SF",
                                    "GR_02_SF",
                                    "GR_03_SF",
                                    "GR_04_SF",
                                    "GR_05_SF"))) %>%
  ggplot(aes(x = ID, y = value, fill = watershed)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "Geomorphic Reach",
       y = "Suitability Index",
       title = "Steelhead Juvenile Rearing 2021") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
sthd_juv_2021_geo

##################################
# Box plot metrics (composite, depth, velocity) by rkm and year #
##################################

# define rkm levels #
rkm_levels = c("RKM_01_MS","RKM_02_MS","RKM_03_MS","RKM_04_MS","RKM_05_MS","RKM_06_MS","RKM_07_MS","RKM_08_MS",
               "RKM_09_MS","RKM_10_MS","RKM_11_MS","RKM_12_MS","RKM_13_MS","RKM_14_MS","RKM_15_MS","RKM_16_MS",
               "RKM_17_MS","RKM_18_MS","RKM_19_MS","RKM_20_MS","RKM_21_MS","RKM_22_MS","RKM_23_MS","RKM_24_MS",
               "RKM_25_MS","RKM_26_MS","RKM_27_MS","RKM_28_MS","RKM_29_MS","RKM_30_MS","RKM_31_MS","RKM_32_MS",
               "RKM_33_MS","RKM_34_MS","RKM_35_MS","RKM_36_MS","RKM_37_MS","RKM_38_MS","RKM_39_MS",
               "RKM_01_NF","RKM_02_NF","RKM_03_NF","RKM_04_NF","RKM_05_NF","RKM_06_NF","RKM_07_NF","RKM_08_NF",
               "RKM_09_NF","RKM_10_NF","RKM_11_NF","RKM_12_NF","RKM_13_NF","RKM_14_NF","RKM_15_NF","RKM_16_NF",
               "RKM_17_NF","RKM_18_NF","RKM_19_NF",
               "RKM_01_SF","RKM_02_SF","RKM_03_SF","RKM_04_SF","RKM_05_SF","RKM_06_SF","RKM_07_SF","RKM_08_SF",
               "RKM_09_SF","RKM_10_SF","RKM_11_SF","RKM_12_SF","RKM_13_SF","RKM_14_SF","RKM_15_SF","RKM_16_SF",
               "RKM_17_SF","RKM_18_SF","RKM_19_SF","RKM_20_SF","RKM_21_SF","RKM_22_SF","RKM_23_SF","RKM_24_SF",
               "RKM_25_SF","RKM_26_SF","RKM_27_SF","RKM_28_SF","RKM_29_SF","RKM_30_SF","RKM_31_SF","RKM_32_SF")

# Chinook juvenile rearing 2019 MS #
chnk_juv_2019_rkm_ms = hsi_raw %>%
  filter(year == "2019",
         scenario == "chnk_juv",
         str_starts(ID, "RKM"),
         str_ends(ID, "MS")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "Mainstem Chinook Juvenile Rearing 2019") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
chnk_juv_2019_rkm_ms

# Chinook juvenile rearing 2019 NF #
chnk_juv_2019_rkm_nf = hsi_raw %>%
  filter(year == "2019",
         scenario == "chnk_juv",
         str_starts(ID, "RKM"),
         str_ends(ID, "NF")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "NF Chinook Juvenile Rearing 2019") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
chnk_juv_2019_rkm_nf

# Chinook juvenile rearing 2019 SF #
chnk_juv_2019_rkm_sf = hsi_raw %>%
  filter(year == "2019",
         scenario == "chnk_juv",
         str_starts(ID, "RKM"),
         str_ends(ID, "SF")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "SF Chinook Juvenile Rearing 2019") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
chnk_juv_2019_rkm_sf

# Chinook juvenile rearing 2021 MS #
chnk_juv_2021_rkm_ms = hsi_raw %>%
  filter(year == "2021",
         scenario == "chnk_juv",
         str_starts(ID, "RKM"),
         str_ends(ID, "MS")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "Mainstem Chinook Juvenile Rearing 2021") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
chnk_juv_2021_rkm_ms

# Chinook juvenile rearing 2021 NF #
chnk_juv_2021_rkm_nf = hsi_raw %>%
  filter(year == "2021",
         scenario == "chnk_juv",
         str_starts(ID, "RKM"),
         str_ends(ID, "NF")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "NF Chinook Juvenile Rearing 2021") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
chnk_juv_2021_rkm_nf

# Chinook juvenile rearing 2021 SF #
chnk_juv_2021_rkm_sf = hsi_raw %>%
  filter(year == "2021",
         scenario == "chnk_juv",
         str_starts(ID, "RKM"),
         str_ends(ID, "SF")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "SF Chinook Juvenile Rearing 2021") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
chnk_juv_2021_rkm_sf

# Chinook spawning 2019 MS #
chnk_spw_2019_rkm_ms = hsi_raw %>%
  filter(year == "2019",
         scenario == "chnk_spw",
         str_starts(ID, "RKM"),
         str_ends(ID, "MS")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "Mainstem Chinook Spawning 2019") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
chnk_spw_2019_rkm_ms

# Chinook spawning 2019 NF #
chnk_spw_2019_rkm_nf = hsi_raw %>%
  filter(year == "2019",
         scenario == "chnk_spw",
         str_starts(ID, "RKM"),
         str_ends(ID, "NF")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "NF Chinook Spawning 2019") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
chnk_spw_2019_rkm_nf

# Chinook spawning 2019 SF #
chnk_spw_2019_rkm_sf = hsi_raw %>%
  filter(year == "2019",
         scenario == "chnk_spw",
         str_starts(ID, "RKM"),
         str_ends(ID, "SF")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "SF Chinook Spawning 2019") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
chnk_spw_2019_rkm_sf

# Chinook spawning 2021 MS #
chnk_spw_2021_rkm_ms = hsi_raw %>%
  filter(year == "2021",
         scenario == "chnk_spw",
         str_starts(ID, "RKM"),
         str_ends(ID, "MS")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "Mainstem Chinook Spawning 2021") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
chnk_spw_2021_rkm_ms

# Chinook spawning 2021 NF #
chnk_spw_2021_rkm_nf = hsi_raw %>%
  filter(year == "2021",
         scenario == "chnk_spw",
         str_starts(ID, "RKM"),
         str_ends(ID, "NF")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "NF Chinook Spawning 2021") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
chnk_spw_2021_rkm_nf

# Chinook spawning 2021 SF #
chnk_spw_2021_rkm_sf = hsi_raw %>%
  filter(year == "2021",
         scenario == "chnk_spw",
         str_starts(ID, "RKM"),
         str_ends(ID, "SF")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "SF Chinook Spawning 2021") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
chnk_spw_2021_rkm_sf

# steelhead juvenile rearing 2019 MS #
sthd_juv_2019_rkm_ms = hsi_raw %>%
  filter(year == "2019",
         scenario == "sthd_juv",
         str_starts(ID, "RKM"),
         str_ends(ID, "MS")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "Mainstem Steelhead Juvenile Rearing 2019") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
sthd_juv_2019_rkm_ms

# steelhead juvenile rearing 2019 NF #
sthd_juv_2019_rkm_nf = hsi_raw %>%
  filter(year == "2019",
         scenario == "chnk_sthd",
         str_starts(ID, "RKM"),
         str_ends(ID, "NF")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "NF Steelhead Juvenile Rearing 2019") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
sthd_juv_2019_rkm_nf

# steelhead juvenile rearing 2019 SF #
sthd_juv_2019_rkm_sf = hsi_raw %>%
  filter(year == "2019",
         scenario == "sthd_juv",
         str_starts(ID, "RKM"),
         str_ends(ID, "SF")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "SF Steelhead Juvenile Rearing 2019") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
sthd_juv_2019_rkm_sf

# steelhead juvenile rearing 2021 MS #
sthd_juv_2021_rkm_ms = hsi_raw %>%
  filter(year == "2021",
         scenario == "sthd_juv",
         str_starts(ID, "RKM"),
         str_ends(ID, "MS")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "Mainstem Steelhead Juvenile Rearing 2021") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
steelhead_juv_2021_rkm_ms

# steelhead juvenile rearing 2021 NF #
sthd_juv_2021_rkm_nf = hsi_raw %>%
  filter(year == "2021",
         scenario == "sthd_juv",
         str_starts(ID, "RKM"),
         str_ends(ID, "NF")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "NF Steelhead Juvenile Rearing 2021") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
sthd_juv_2021_rkm_nf

# Steelhead juvenile rearing 2021 SF #
sthd_juv_2021_rkm_sf = hsi_raw %>%
  filter(year == "2021",
         scenario == "sthd_juv",
         str_starts(ID, "RKM"),
         str_ends(ID, "SF")) %>%
  mutate(ID = factor(ID, levels = rkm_levels)) %>%
  ggplot(aes(x = ID, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~ metric, nrow = 3) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Suitability Index",
       title = "SF Steelhead Juvenile Rearing 2021") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
sthd_juv_2021_rkm_sf

#### Geo Reach Summary Table Script #####
geo_reach = read_csv(here("analysis/data/raw_data/reach_partitions/Geo_reaches_corrected.csv")) %>%
  rename(Watershed = Strm_Name,
  min = From_RKM,
  max = To_RKM,
  'Geomorphic Reach' = Reach_ID) %>%
  relocate("Watershed", "Geomorphic Reach", "min", "max") %>%
  select(Watershed, 'Geomorphic Reach', min, max)

geo_reach$`Geomorphic Reach`= ordered(geo_reach$`Geomorphic Reach`, levels = c("GR_01_MS",
                                                                               "GR_02_MS",
                                                                               "GR_03_MS",
                                                                               "GR_04_MS",
                                                                               "GR_05_MS",
                                                                               "GR_01_NF",
                                                                               "GR_02_NF",
                                                                               "GR_03_NF",
                                                                               "GR_04_NF",
                                                                               "GR_05_NF",
                                                                               "GR_01_SF",
                                                                               "GR_02_SF",
                                                                               "GR_03_SF",
                                                                               "GR_04_SF",
                                                                               "GR_05_SF"))
geo_reach = geo_reach %>%
  arrange(factor(`Geomorphic Reach`))
