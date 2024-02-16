library('dplyr') # for data wrangling (mutate(), %>%, etc.)
library('tidyr') # for data wrangling (unnest(), pivot_*, etc.)
library('purrr') # for functional programming (map_***(), etc.)
library('furrr') # for futures and parallel computing
library('ctmm')  # for movement models

# run time using 8 cores: TBD
T_START <- Sys.time()

#' `https://github.com/ctmm-initiative/ctmmlearn/blob/main/ctmm_error.R`
# goats are the only species with calibration data
goat_uere <-
  as.telemetry('data/tracking-data/goat-calibration-data.csv') %>%
  uere.fit()

# running models separately to avoid having to re-run previous code
plan(multisession, workers = 8)
d <-
  readRDS('data/tracking-data/all-tracking-data-cleaned-2023-11-17-16-04.rds') %>%
  mutate(
    # convert data frames to telemetry objects
    tel = map(tel, \(x) as.telemetry(x, mark.rm = TRUE)),
    # add calibration-informed error or a reasonable guess
    tel = if_else(condition = species == 'Oreamnos americanus',
                  true = map(tel, \(x) {
                    uere(x) <- goat_uere
                    return(x)
                  }),
                  false = map(tel, \(x) {
                    # not changing default errors: assuming errors of 10 m
                    prior <- uere(x) # extract calibration object
                    prior$DOF[] <- 2 # low DOF for large uncertainty
                    uere(x) <- prior # assign prior to the data
                    return(x)
                  })),
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

# Warning message:                                   
#   There were 8 warnings in `mutate()`.
# The first warning was:
#   ℹ In argument: `ud = map(...)`.
# Caused by warning in `CTMM$error > 0 & !is.na(UERE.DOF)`:
#   ! longer object length is not a multiple of shorter object length
# ℹ Run dplyr::last_dplyr_warnings() to see the 7 remaining warnings.

T_END <- Sys.time()

T_END - T_START
