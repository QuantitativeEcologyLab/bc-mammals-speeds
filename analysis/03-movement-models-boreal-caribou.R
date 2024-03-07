library('dplyr') # for data wrangling (mutate(), %>%, etc.)
library('tidyr') # for data wrangling (unnest(), pivot_*, etc.)
library('purrr') # for functional programming (map_***(), etc.)
library('furrr') # for futures and parallel computing
library('ctmm')  # for movement models

# run time using 13 cores on laptop: 3.6 days
T_START <- Sys.time()

# running models separately to avoid having to re-run previous code
CORES <- parallel::detectCores(logical = FALSE) - 1
plan(multisession, workers = CORES)

d <-
  readRDS('data/tracking-data/all-tracking-data-cleaned-2024-02-22-13-49.rds') %>%
  filter(dataset_name == 'Rangifer_tarandus_boreal') %>%
  mutate(
    # convert data frames to telemetry objects
    tel = map(tel, \(x) as.telemetry(x, mark.rm = TRUE)),
    # add calibration-informed error or a reasonable guess
    tel = map(tel, \(x) {
      # not changing default errors: assuming errors of 10 m
      prior <- uere(x) # extract calibration object
      prior$DOF[] <- 2 # low DOF for large uncertainty
      uere(x) <- prior # assign prior to the data
      return(x)
    }),
    variogram = future_map(tel, \(x) ctmm.guess(data = x,
                                                CTMM = ctmm(error = TRUE),
                                                interactive = FALSE),
                           .progress = TRUE))

d <- mutate(
  d,
  movement_model = future_map(
    .x = 1:n(),
    \(i) ctmm.select(data = tel[[i]], CTMM = variogram[[i]]),
    .progress = TRUE)) # print overall progress
saveRDS(d, paste0('models/movement-models-', Sys.Date(), '.rds'))

d <- mutate(
  d,
  ud = map(
    1:n(),
    \(i) akde(data = tel[[i]], CTMM = movement_model[[i]], weights = TRUE),
    .progress = TRUE))
saveRDS(d, paste0('models/movement-models-akdes-', Sys.Date(), '.rds'))

T_END <- Sys.time()

T_END - T_START
