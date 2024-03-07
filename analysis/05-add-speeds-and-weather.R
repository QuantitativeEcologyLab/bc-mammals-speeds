library('sf')      # for working with spatial data
library('terra')   # for working with raster data
library('ncdf4')   # for nc datasets
library('dplyr')   # for data wrangling (e.g., mutate, %>%, ...)
library('purrr')   # for functional programming (e.g., map_***())
library('furrr')   # for parallel functional programming (e.g., future_map())
library('ggplot2') # for fancy plots
library('ctmm')    # for movement modeling
source('functions/detrended_speeds.R') # to remove baseline noise in speed

theme_set(theme_bw())

# add weather data and speeds to the telemetry data ----
# see: https://confluence.ecmwf.int/display/CKB/ERA5-Land%3A+data+documentation

#' to parallelize using `furrr`
CORES <- availableCores(logical = FALSE) - 4
# plan(multisession, workers = CORES)
d <-
  readRDS('models/movement-models-2024-03-05.rds') %>%
  filter(map_chr(movement_model, class) != 'NULL') %>%
  filter(dataset_name == 'Rangifer_tarandus_boreal') %>%
  select(! tel) %>% # previously dropped the temperature column...
  left_join(readRDS('data/tracking-data/all-tracking-data-cleaned-2024-02-22-13-49.rds') %>%
              select(animal, tel),
            by = 'animal') %>%
  mutate(tel = map(tel, \(.tel) as.telemetry(.tel,
                                             mark.rm = TRUE, # rm outliers
                                             keep = TRUE)), # temperature
         annotated = future_imap(tel, \(.tel, i) { # add speeds and weather
           STATUS <<- paste0('Animal ', i, ', ', dataset_name[i])
           # telemetry as unprojected spatial points object
           # .tel_sp <- st_transform(SpatialPoints.telemetry(.tel),
           #                         '+proj=longlat')
           
           # estimate speeds
           speeds_data <-
             detrend.speeds(DATA = .tel, CTMM = movement_model[[i]]) %>%
             transmute(speed_low = low, speed_est = est, speed_high = high)
           
           # add weather data
           weather_data <-
             data.frame(.tel) %>% # telemetry as data frame
             #'   #' *note*: `map()` cannot accept rasters as input
             #'   mutate(
             #'     nearest_hour = timestamp %>%
             #'       lubridate::as_datetime() %>%
             #'       round(units = 'hours') %>%
             #'       format(format = '%Y-%m-%d-%H'),
             #'     file_name = paste0('data/ecmwfr-data/', nearest_hour, '-data.nc'),
             #'     # temperature (currently degrees Kelvin)
             #'     temperature = imap_dbl(
             #'       file_name,
             #'       \(fn, ..i) {
           #'         if(file.exists(fn)) {
           #'           fn %>%
           #'             raster(varname = 't2m') %>%
           #'             raster::extract(.tel_sp[..i])
           #'         } else {
           #'           NA_real_
           #'         }
           #'       })) %>%
           #'   # only keep relevant columns
           select(c(timestamp, longitude, latitude, temperature)) #%>%
           #'   mutate(temperature = temperature - 273.15) # from Kelvin to Celsius
           
           # return weather and speed data
           return(bind_cols(weather_data, speeds_data))
         })) %>%
  select(animal, species, dataset_name, annotated) %>%
  tidyr::unnest(annotated)

plan(sequential)

# check how many temperature values are NA
filter(d, is.na(temperature))

# add AKDE weights
d_akde <- readRDS('models/movement-models-akdes-2024-03-05.rds') %>%
  mutate(tel = map2(ud, tel, \(.ud, .tel) {
    .tel %>%
      data.frame() %>%
      # ~ HR effective sample size * AKDE weight
      mutate(weight = unique(.ud$DOF.H) * .ud$weights) %>%
      select(timestamp, weight)
  })) %>%
  select(animal, tel) %>%
  tidyr::unnest(tel)

d <- left_join(d, d_akde, by = c('animal', 'timestamp'))

saveRDS(d, paste0('data/standardized-speeds-weights-', Sys.Date(), '.rds'))

# check relationships with speed estimates
ggplot(d, aes(temperature, speed_est)) +
  facet_wrap(~ species) +
  geom_hex() +
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 5), color = 'red3') +
  labs(x = 'Temperature (\u00B0C)', y = 'Estimated speed (m/s)')
