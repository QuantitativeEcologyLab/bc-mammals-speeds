library('sf')      # for spatial objects
library('dplyr')   # for data wrangling

prov <- canadianmaps::PROV %>%
  st_geometry() %>%
  st_transform('EPSG:3005')

bc <- filter(canadianmaps::PROV, PRENAME == 'British Columbia') %>%
  st_geometry() %>%
  st_transform('EPSG:3005')
