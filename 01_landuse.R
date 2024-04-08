library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)

r <- list(sc500,sc750, sc1000,sc1250,sc1500,sc1750,sc2000)  ## create list with raw QGIS data of all radii

### HABITAT CALCULATION

land_fun <- function(x){ x %>% rename(Site = Landscape3) %>%
    mutate(area_ha = area/10000) %>%
    group_by(Site, Habitat) %>%
    summarise(sum = sum(area_ha)) %>%
    filter(!is.na(Site)) %>%
    pivot_wider(names_from = Habitat, values_from = sum, values_fill = 0) %>%
    mutate(SNH = semi_natur + Fallow + Other_AUM + Grassy_str + Flower_fieBS2 + Flower_fieBS12/2 + Flower_fie,
           Ann.fl = Flower_fieBS11 + Flower_fieBS12/2) %>%
    rename(Org.farm = CropBV1)
}

radii <- r %>% lapply(land_fun)


# 
land_prop <- function(x) { x %>% rename(Org.farm = CropBV1) %>%  #BV1 means organic agriculture in Germany
  dplyr::select(Site, Org.farm, SNH, Ann.fl, OSR) %>% # selecting the three AEM + OSR
  mutate(across(Org.farm:OSR, function(x) x*100/(pi*radius^2/10000))) # calculating the % of each variable in 2km radius
}

land <- radii %>% lapply(land_prop)

save(land, file = 'Landuse.RData')

  
