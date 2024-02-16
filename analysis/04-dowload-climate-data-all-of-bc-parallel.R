library('ctmm')   # for movement data
library('ecmwfr') # for data from EU Centre for Medium-range Weather Forecasts
library('dplyr')  # for data wrangling
library('tidyr')  # for data wrangling
library('purrr')  # for functional programming
library('sf')     # for working with spatial data
library('ctmm')   # for working with telemetries

# ensure the working directory is correct before running anything
if(! grepl('GitHub/bc-mammals-speeds', getwd())) stop('WRONG WD!\n')

# get your login info from the copernicus website, if necessary
if(FALSE) wf_set_key(user = '', key = '', service = 'cds')

if(! dir.exists('data/ecmwf-data')) dir.create('data/ecmwf-data')

d <- readRDS('data/tracking-data/all-tracking-data-cleaned-2023-11-17-16-04.rds')

# using a separate directory for each dataset to run parallel downloads
DIR <- 'data/ecmwf-data/bc-ecmwf-data/parallel-test/'

if(! dir.exists(DIR)) dir.create(DIR)

# bounding box for the dataset
ext <- d %>%
  pull(tel) %>%
  map_dfr(\(.t) { # unproject all points back to latlong
    .t <- as.telemetry(.t, mark.rm = TRUE)
    .t %>%
      SpatialPoints.telemetry() %>%
      st_as_sf() %>%
      st_set_crs(.t@info$projection) %>% # extract tel's projection
      st_transform('+proj=longlat')
  }) %>%
  st_bbox() %>% # find bounding box
  `[`(c('ymin', 'xmin', 'ymax', 'xmax')) #' required by `wf_request()`

ext <- d %>%
  select(tel) %>%
  tidyr::unnest(tel) %>%
  summarize(ymin = min(location.lat, na.rm = TRUE),
            xmin = min(location.long, na.rm = TRUE),
            ymax = max(location.lat, na.rm = TRUE),
            xmax = max(location.long, na.rm = TRUE)) %>%
  as.numeric()

# find which times are needed
times <-
  unnest(d, tel) %>%
  pull(timestamp) %>%
  as.POSIXct() %>%
  round(units = 'hours') %>%
  unique() %>%
  as.character() %>%
  sort()
head(times) # check format

#' *NOTE:* if the download fails due to an internal server error or an error
#'         similar to the following:
#'         - `No encoding supplied: defaulting to UTF-8.`,
#'         - `Error in if (private$status == "completed") { :`
#'             `argument is of length zero`,
#'         try again after a minute or two.

timestamp <- '2012-01-25 01:00:00'

#' if the download fails, run the following line to start from the failed time
if(class(timestamp) == 'character') { #' if `timestamp` is not a function
  times[1]
  times <- times[times >= timestamp] # drop the downloaded times 
  times[1] # print the first timestamp for a visual check
}

hist(as.POSIXct(times), breaks = 'years') # visualize all remaining times

WORKERS <- min(parallel::detectCores(logical = FALSE) - 2, 10)
WORKERS

for(folder in paste0(DIR, 1:WORKERS %% max(WORKERS))) {
  if(! dir.exists(folder)) dir.create(path = folder)
  rm(folder)
}

# to create a new request:
#' *1* copy API from Copernicus website
#' *2* paste it and highlight it and use `Addins` > `MARS to list`
#' *3* edit as needed to run a test download or download all data  
requests <-
  map2(times, 1:length(times) %% WORKERS, function(.timestamp, .i) {
  request <- list(
    variable = c('2m_temperature'),
    year = format(as.POSIXct(timestamp), '%Y'),
    month = format(as.POSIXct(timestamp), '%m'),
    day = format(as.POSIXct(timestamp), '%d'),
    time = paste0(format(as.POSIXct(timestamp), '%H'), ':00'),
    area = ext, 
    format = 'netcdf.zip',
    dataset_short_name = 'reanalysis-era5-land',
    target = paste0(.i, '/', .timestamp, '-download.netcdf.zip'))
})

wf_request_batch(request_list = requests, workers = WORKERS,
                 user = '165099', path = DIR, time_out = 1 %#% 'week')
