uww_stream_temp.gpk contains 15 layers (5 species/life stages x 3 temperature scenarios.

Temperature scenarios include:
* Recent Historic = A historical composite scenario representing the 10-year average August mean stream temperatures for 2002-2011 (Isaak et al., 2017)
* 2040 Projected = A future August mean stream temperature scenario based on average projected changes in August air temperature and stream discharge for the 2040s (2030-2059). Projected changes in streams accounted for differential sensitivity among streams so that cold streams warm less than warm streams (Isaak et al., 2017).
* 2080 Projected = A future August mean stream temperature scenario based on average projected changes in August air temperature and stream discharge for the 2080s (2070-2099). Projected changes in streams accounted for differential sensitivity among streams so that cold streams warm less than warm streams (Isaak et al., 2017).

Species and life stages include:
* Chinook salmon spawning (chnk_spw)
* Chinook salmon summer parr rearing (chnk_juv)
* Steelhead summer parr rearing (sthd_juv)
* Bull trout (bull)
* Pacific lamprey (lamp)

Fields include:
spec_ls: The species and life stage
scenario: The NorWeST scenario (Isaak et al., 2017)
in_domain: Does the reach fall within the spatial distribution of the species, as provided by NorWeST (note: no spatial domain is provided by NorWest for Pacific lamprey, and thus, all values = TRUE)
reach_leng: The reach length in meters
temp_c: The modeled or predicted mean August stream temperature (Celcius)
`Within Optimum`:`Above Acute`: Does the temperature fall within optimum, or above optimum, maximum, and acute temperature thresholds for each species and life stage.
