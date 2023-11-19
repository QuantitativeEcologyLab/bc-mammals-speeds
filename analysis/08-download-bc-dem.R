library('sp')           # for spatial data
library('sf')           # for spatial data
library('dplyr')        # for data wrangling
library('canadianmaps') # for shapefile of BC
library('elevatr')      # for downloading DEM (using version 0.99.0)
library('terra')        # for working with rasters

# shapefile of British Columbia
bc_shp <-
  st_as_sf(PROV) %>% # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() %>% # extract boundaries only
  st_as_sf() # convert from sfc to sf object

ZOOM <- 6 #' resolution level for the DEM (see `?get_elev_raster`)

bc_dem <-
  get_elev_raster(locations = bc_shp, z = ZOOM, clip = 'locations') %>%
  rast() #' `elevatr` v 0.99.0 still returs a `RasterLayer` object

plot(bc_dem)
plot(bc_shp, add = TRUE)

# highest point in BC is mount fairweather at ~ 4e3 m
#' `ZOOM > 6` results in excessively high elevation values
max(bc_dem)
get_elev_raster(locations = bc_shp, z = 7, clip = 'locations') %>%
  rast() %>% #' `elevatr` v 0.99.0 still returs a `RasterLayer` object
  max()

terra::writeRaster(bc_dem, paste0('data/bc-dem-z', ZOOM, '.tif'))

# ensure res is not lower than that of the resource rasters
if(all(res(bc_dem) <=
       res(rast('data/')))) {
  cat('DEM resolution is sufficiently high.\n')
} else {
  stop('DEM RESOLUTION IS TOO LOW!')
}
