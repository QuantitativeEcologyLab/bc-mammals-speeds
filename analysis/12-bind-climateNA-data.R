library('purrr')     # for functional programming (map_***())
library('dplyr')     # for dara wrangling (mutate, transmute, etc.)
library('tidyr')     # for data wrangling (pivot_longer, pivot_wider, nest)
library('stringi')   # for working with strings
library('lubridate') # for working with dates

# to bind all climate projections from each year together into a single RDS file
d <-
  # import all files
  map_dfr(
    list.files(
      'ClimateNA_v742/bc-dem-z6/projection-data',
      full.names = TRUE, # include folder names in file name
      pattern = '@') %>%
      head(), # only yearly datasets have a "@"
    \(.fname) {
      # readr::read_csv(.fname, col_types = '?', progress = FALSE) %>%
      data.table::fread(.fname, showProgress = FALSE) %>%
        mutate(file = .fname) # add column of filename for scenario & year
    }, .progress = TRUE) %>%
  # add scenario and year columns
  mutate(scenario = substr(file,
                           start = stri_locate_last(file, regex = '/')[1] + 1,
                           stop = stri_locate_first(file, regex = '@')[1] - 1),
         year = substr(file,
                       start = stri_locate_first(file, regex = '@')[1] + 1,
                       stop = nchar(file) - nchar('.csv'))) %>%
  # only keep necessary columns
  select(scenario, year, Latitude, Longitude, Elevation,
         Tave01:Tave12, Tmin01:Tmin12, Tmax01:Tmax12, PPT01:PPT12)# %>%

#' mean temperature is measured as `Tave = (Tmax + Tmin) / 2`
#' `https://www.prism.oregonstate.edu/documents/PRISM_datasets.pdf`
#' *can simulate hourly temperature using a smooth sinusoidal function*
with(slice_sample(d, n = 5e4),
     plot(Tave01 - Tmin01, Tmax01 - Tave01, col = '#00000030', pch = '.'))
for(a in c(-0.1, 0, 0.1)) abline(a = a, b = 1, col = 'red'); rm(a)

# PPT is total precip within each month, in mm
#' simulating hourly `PPT` is harder because there's no measure of scale
#' could simulate from an exponential distribution with mean = 1/lambda
quantile(d$PPT01)
tibble(y = seq(0.01, 100, by = 0.1),
       shape = 0.75,
       scale = 10 / shape,
       dens = dgamma(y, shape = shape, scale = scale)) %>%
       plot(dens ~ y, ., type = 'l', ylim = c(0, max(.$dens)))

d <- d %>%
  # pivot to long format
  pivot_longer(-c(scenario, year, Latitude, Longitude, Elevation),
               names_to = 'parameter', values_to = 'value') %>%
  # extract time and parameter columns 
  mutate(month = map_chr(parameter,
                         \(.chr) substr(.chr, nchar(.chr) - 1, nchar(.chr))),
         dec_date = decimal_date(date(paste(year, month, '15', sep = '-'))),
         month = as.numeric(month),
         year = as.numeric(year),
         parameter = map_chr(parameter,
                             \(.chr) substr(.chr, 1, nchar(.chr) - 2))) %>%
  # create separate columns for temperature and precipitation
  pivot_wider(names_from = parameter, values_from = value) %>%
  # convert monthly total precip to hourly total precip
  mutate(first_day = as.Date(paste(year, month, '01', sep = '-')),
         next_month = if_else(month != '12', as.numeric(month + 1), 1),
         next_year = if_else(month != '12', year, year + 1),
         last_day = as.Date(paste(next_year, next_month, '01', sep = '-')),
         hours = as.numeric((last_day - first_day)) * 24,
         tot_precip = PPT / hours) %>%
  # drop temporary columns
  select(-c(first_day, next_month, next_year, last_day, hours, PPT)) %>%
  # change to names used in the models
  rename(mean_temperature = Tave,
         min_temperature = Tmin,
         max_temperature = Tmax,
         latitude = Latitude,
         longitude = Longitude,
         elevation = Elevation) %>%
  # place month and decimal date columns after the year column
  relocate(c(month, dec_date), .after = year)

# save the output for later use
saveRDS(d, 'data/climate-yearly-projections.rds')
