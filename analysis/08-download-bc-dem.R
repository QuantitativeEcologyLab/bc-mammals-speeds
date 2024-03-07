library('sf')           # for spatial data
library('dplyr')        # for data wrangling
library('canadianmaps') # for shapefile of BC
library('elevatr')      # for downloading DEM (using version 0.99.0)
library('terra')        # for working with rasters
source('data/bc-shapefile.R')

ZOOM <- 6 #' resolution level for the DEM (see `?get_elev_raster`)

bc_dem <-
  get_elev_raster(locations = bc_unproj, z = ZOOM, clip = 'locations') %>%
  rast() #' `elevatr` v 0.99.0 still returs a `RasterLayer` object

plot(bc_dem)
plot(bc_unproj, add = TRUE)

if(FALSE) {
  # highest point in BC is mount fairweather at ~ 4e3 m
  #' `ZOOM > 6` results in excessively high elevation values
  max(values(bc_dem), na.rm = TRUE)
  get_elev_raster(locations = bc_unproj, z = 7, clip = 'locations') %>%
    rast() %>% #' `elevatr` v 0.99.0 still returs a `RasterLayer` object
    values() %>%
    max(na.rm = TRUE)
  
  # res is close enough
  res(bc_dem)
  res(rast('data/resource-rasters/bc-forest.tif'))
  res(rast('data/resource-rasters/bc-distance-from-water.tif'))
}

terra::writeRaster(bc_dem, paste0('data/bc-dem-z', ZOOM, '.tif'))

# download dem of buffered BC
bc_unproj_buff <- bc_unproj %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_buffer(1e5)
plot(bc_unproj_buff)
plot(bc_unproj, add = TRUE)

get_elev_raster(locations = bc_unproj_buff, z = ZOOM,
                clip = 'bbox') %>%
  rast() %>% #' `elevatr` v 0.99.0 still returs a `RasterLayer` object
  terra::writeRaster(paste0('data/bc-buffered-dem-z', ZOOM, '.tif'))
