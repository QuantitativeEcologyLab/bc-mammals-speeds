library('dplyr') # for data wrangling
library('tidyr') # for data wrangling
library('purrr') # for functional programming
library('sf')    # for working with spatial data
library('terra') # for raster data
source('data/bc-shapefile.R')

bounds <-
  readRDS('data/tracking-data/all-tracking-data-cleaned-2024-02-22-13-49.rds') %>%
  filter(dataset_name == 'Rangifer_tarandus_boreal') %>%
  unnest(tel) %>%
  select(location.long, location.lat) %>%
  st_as_sf(coords = c('location.long', 'location.lat')) %>%
  st_set_crs('+proj=longlat') %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_buffer(1e4)

# original rasters from http://www.earthenv.org/landcover
e <- rast('data/resource-rasters/bc-buffered-dem-z6.tif')
plot(e)
plot(bc_unproj, add = TRUE)
plot(bounds, add = TRUE)

# forest raster
f <- rast('data/resource-rasters/uncropped-rasters/consensus_full_class_1.tif') %>%
  crop(bounds)
plot(f, add = TRUE)
writeRaster(f, 'data/resource-rasters/boreal-caribou/forest.tif')

# raster of open water
layout(matrix(1:8, ncol = 2, byrow = TRUE))
w <- rast('data/resource-rasters/uncropped-rasters/consensus_full_class_12.tif') %>%
  crop(st_buffer(bounds, 1e4))
plot(w, main = 'Percent water')
plot(bounds, add = TRUE)
hist(w, breaks = 100, main = 'Percent water')

w01 <- ceiling(w / 100) # convert anything above 0 to a 1
plot(w01, main = 'Yes/No water')
hist(w01, main = 'Yes/No water')

w1 <- classify(w01, cbind(0, NA))
plot(w1, main = 'Water only', col = 'blue')
hist(w1, main = 'Water only')

dist <- distance(w1)
plot(dist, main = 'Distance from nearest water')
hist(dist, main = 'Distance from nearest water')
writeRaster(crop(dist, bounds), 'data/resource-rasters/boreal-caribou/distance-from-water.tif')
