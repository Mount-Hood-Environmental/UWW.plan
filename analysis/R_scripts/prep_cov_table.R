# Authors: Mike Ackerman
# Purpose: Prep QRF covariate table from QRFcapacity repo
# Created: 2/15/2022
# Last Modified: 2/15/2022
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(magrittr)
library(here)

#-----------------------------------------------------------------
# set NAS prefix, depending on operating system
if(.Platform$OS.type != 'unix') {nas_prefix = "S:"}

#-----------------------------------------------------------------
# load QRF model fits
load(here("../QRFcapacity/output/modelFits/qrf_juv_summer.rda"))
juv_sum_chnk_covs = qrf_mods$Chinook$importance
juv_sum_sthd_covs = qrf_mods$Steelhead$importance

load(here("../QRFcapacity/output/modelFits/qrf_juv_winter.rda"))
juv_win_chnk_covs = qrf_mods$Chinook$importance
juv_win_sthd_covs = qrf_mods$Steelhead$importance

load(here("../QRFcapacity/output/modelFits/qrf_redds.rda"))
redds_chnk_covs = qrf_mods$Chinook$importance
redds_sthd_covs = qrf_mods$Steelhead$importance

# clean up environment
rm(fish_hab,
   hab_avg,
   hab_data,
   qrf_mod_df,
   qrf_mods,
   sel_hab_mets)

# Create Habitat Covariate Table
# Juvenile Summer Chinook
hab_cov_tbl = as_tibble(juv_sum_chnk_covs, rownames = "Covariate") %>%
  mutate(`Juv Sum Chnk` = dense_rank(desc(IncNodePurity))) %>%
  select(-IncNodePurity) %>%
  # Juvenile Summer Steelhead
  full_join(as_tibble(juv_sum_sthd_covs, rownames = "Covariate") %>%
            mutate(`Juv Sum Sthd` = dense_rank(desc(IncNodePurity))) %>%
            select(-IncNodePurity)) %>%
  # Juvenile Winter Chinook
  full_join(as_tibble(juv_win_chnk_covs, rownames = "Covariate") %>%
            mutate(`Juv Win Chnk` = dense_rank(desc(IncNodePurity))) %>%
            select(-IncNodePurity)) %>%
  # Juvenile Winter Steelhead
  full_join(as_tibble(juv_win_sthd_covs, rownames = "Covariate") %>%
            mutate(`Juv Win Sthd` = dense_rank(desc(IncNodePurity))) %>%
            select(-IncNodePurity)) %>%
  # Redds Chinook
  full_join(as_tibble(redds_chnk_covs, rownames = "Covariate") %>%
            mutate(`Redds Chnk` = dense_rank(desc(IncNodePurity))) %>%
            select(-IncNodePurity)) %>%
  # Redds Steelhead
  full_join(as_tibble(redds_sthd_covs, rownames = "Covariate") %>%
            mutate(`Redds Sthd` = dense_rank(desc(IncNodePurity))) %>%
            select(-IncNodePurity)) %>%
  left_join(hab_dict %>%
              select(ShortName,
                     Name,
                     DescriptiveText,
                     MetricCategory),
            by = c("Covariate" = "ShortName")) %>%
  distinct() %>%
  # some cleaning
  mutate(`Juv Win Chnk` = ifelse(Covariate == "Q", 2, `Juv Win Chnk`),
         `Juv Win Sthd` = ifelse(Covariate == "Q", 1, `Juv Win Sthd`),
         `Juv Win Chnk` = ifelse(Covariate == "LWFreq_Wet", 10, `Juv Win Chnk`),
         `Juv Win Sthd` = ifelse(Covariate == "LWFreq_Wet", 10, `Juv Win Sthd`)) %>%
  filter(!Covariate %in% c("Discharge", "LWCount")) %>%
  select(Covariate,
         Name,
         MetricCategory,
         everything(),
         DescriptiveText) %>%
  arrange(MetricCategory,
          Covariate) %>%
  rename(`Metric Category` = MetricCategory,
         Description = DescriptiveText)

# save results
save(hab_cov_tbl,
     file = here("analysis/data/derived_data/hab_cov_tbl.rda"))






