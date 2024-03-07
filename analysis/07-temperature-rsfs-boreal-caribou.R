library('mgcv')  # for GAMs
library('dplyr') # for data wrangling
library('tidyr') # for data wrangling
library('sf')    # for spatial data
library('terra') # for rasters

K <- 1e6 # scaling constant for weights

# import tracking data
d <-
  readRDS('data/tracking-data/all-tracking-data-cleaned-2024-02-22-13-49.rds') %>%
  filter(dataset_name == 'Rangifer_tarandus_boreal') %>%
  unnest(tel) %>%
  filter(temperature < 40) %>%
  filter(! grepl('SCEK014', animal)) %>%
  select(animal, location.long, location.lat, temperature)

locs <- d %>%
  select(location.long, location.lat, animal) %>%
  st_as_sf(coords = c('location.long', 'location.lat'))

bounds <- locs %>%
  group_by(animal) %>%
  st_set_crs('+proj=longlat') %>%
  as_Spatial() %>%
  adehabitatHR::mcp(percent = 100) %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_union() %>%
  st_as_sf()

wide_bounds <- st_buffer(bounds, 3e4)

# import rasters of resources
f <- rast('data/resource-rasters/boreal-caribou/forest.tif') %>%
  crop(wide_bounds) %>%
  mask(wide_bounds)
e <- rast('data/resource-rasters/bc-buffered-dem-z6.tif') %>%
  crop(wide_bounds) %>%
  mask(wide_bounds)
w <- rast('data/resource-rasters/boreal-caribou/distance-from-water.tif') %>%
  crop(wide_bounds) %>%
  mask(wide_bounds)

if(FALSE) {
  # some exploratory behavior going up the mountain
  # AKDE weights should help fix the issue
  layout(t(1:3))
  plot(f)
  # plot(locs, add = TRUE)
  plot(bounds, add = TRUE)
  plot(e)
  plot(bounds, add = TRUE)
  plot(w)
  plot(bounds, add = TRUE)
  layout(1)
}

# data frame of observed locations ----
d_1 <- d %>%
  mutate(forest_perc = extract(f, locs) %>%
           pull(2),
         elevation_m = extract(e, locs) %>%
           pull(2),
         dist_water_m = extract(w, locs) %>%
           pull(2)) %>%
  mutate(detected = 1,
         weight = 1 * K)

# data frame of null locations ----
range(d$temperature, na.rm = TRUE)
null_temperatures <- seq(-33, 40, length.out = 100)

d_0 <-
  as.data.frame(f, xy = TRUE) %>%
  as_tibble() %>%
  rename(forest_perc = consensus_full_class_1) %>%
  mutate(elevation_m = st_as_sf(tibble(x, y), coords = c('x', 'y')) %>%
           extract(x = e, y = .) %>%
           pull(2),
         dist_water_m = st_as_sf(tibble(x, y),
                                 coords = c('x', 'y')) %>%
           extract(x = w, y = .) %>%
           pull(2)) %>%
  rename(location.long = x, location.lat = y) %>%
  filter(! is.na(forest_perc + elevation_m + dist_water_m)) %>%
  bind_rows(., ., .) %>% # tripling the number of zeros has little effect
  mutate(detected = 0,
         weight = 1,
         temperature = sample(x = null_temperatures, size = n(),
                              replace = TRUE))

nrow(d_0) / nrow(d_1)

d_full <- bind_rows(d_1, d_0)

# check coverage ----
if(FALSE) {
  layout(matrix(1:6, ncol = 2))
  hist(d_full$forest_perc, breaks = 10)
  hist(d_full$elevation_m, breaks = 10)
  hist(d_full$dist_water_m, breaks = 10)
  hist(d_1$forest_perc, xlim = range(d_full$forest_perc), breaks = 10)
  hist(d_1$elevation_m, xlim = range(d_full$elevation_m), breaks = 10)
  hist(d_1$dist_water_m, xlim = range(d_full$dist_water_m), breaks = 10)
  layout(1)
}

# fit the RSF ----
plot(density(d_1$elevation_m, bw = 25), xlim = range(d_full$elevation_m),
     col = 'blue')
lines(density(d_0$elevation_m, bw = 25), xlim = range(d_full$elevation_m),
     col = 'red')

#' - adding `log()` gives too much leverage to low elevations and
#' distances from water.
#' - dividing `detected` by `K` and adding `K` in the weights does not
#' improve the model fit
#' - adding the AKDE weights makes everything flat
rsf <- bam(detected ~
             s(forest_perc, k = 4) +
             s(elevation_m, k = 4) +
             s(sqrt(dist_water_m), k = 4) +
             ti(forest_perc, temperature, k = 4, bs = 'ds') +
             ti(elevation_m, temperature, k = 4, bs = 'ds') +
             ti(sqrt(dist_water_m), temperature, k = 4, bs = 'ds'),
           family = poisson(link = 'log'),
           data = d_full,
           # weights = weight,
           method = 'fREML',
           discrete = TRUE,
           control = gam.control(trace = TRUE))

plot(rsf, pages = 1, scheme = c(1, 1, 1, 3, 3, 3), scale = 0)
summary(rsf)
saveRDS(rsf, paste0('models/rsf-boreal-caribou-', Sys.Date(), '.rds'))

if(FALSE) {
  layout(matrix(1:4, ncol = 2))
  gam.check(rsf, type = 'pearson')
  layout(1)
}
