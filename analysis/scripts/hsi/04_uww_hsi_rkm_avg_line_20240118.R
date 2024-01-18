## Line Plot for HHS by RKM ##
## Tulley Mackey ##

# load libraries

library(tidyverse)
library(here)
library(gridExtra)
library(ggmap)
library(sf)
library(reshape2)
library(readr)
library(dplyr)

# upload hsi metric raw files for each species/life stage and year into one df
hsi_avg = list.files(here("analysis/hsi_outputs/csvs"), pattern = "*.csv", full.names = T) %>%
  map_df(~read_csv(.)) %>%

hsi_avg_ms = hsi_avg %>%
  filter(str_starts(ID,"RKM"),
         str_ends(ID, "MS")) %>%
  mutate(rkm = c(1:39))
  ggplot(aes(x = ))










# plot
ggplot(aes(x = fct_rev(geo_reach))) +
  geom_line(aes(y = WUA, group = 1), color = "royalblue4", size = 1.1) +
  geom_line(aes(y = HHS / coeff, group = 1), color = "orangered2", size = 1.1) +
  #geom_point(aes(y = WUA)) +
  #geom_bar(aes(y = HHS / coeff), stat = "identity",
  #         alpha = 0.5, fill = "cornflowerblue") +
  scale_y_continuous(sec.axis = sec_axis(~. * coeff,
                                         name = "Hydraulic Habitat Suitability (HHS)",
                                         breaks = seq(0, 1, by = 0.25))) +
  theme_bw() +
  labs(title = "Lower Lemhi",
       x = "Geomorphic Reach",
       y = expression("Weighted Usable Area (WUA) m"^{2})) +
  facet_grid(species ~ scenario) +
  theme(axis.text.x = element_text(angle = -45, hjust = -0.05, vjust = 0.9, size = 8),
        axis.title.y = element_text(colour = "royalblue4"),
        axis.title.y.right = element_text(colour = "orangered2"))
llem_hsi_p1
