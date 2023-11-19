library('dplyr') # for data wrangling (mutate(), %>%, etc.)
library('tidyr') # for data wrangling (unnest(), pivot_*, etc.)
library('purrr') # for functional programming (map_***(), etc.)
library('ctmm')  # for movement models

#' `https://github.com/ctmm-initiative/ctmmlearn/blob/main/ctmm_error.R`
# goats are the only species with calibration data
goat_uere <-
  as.telemetry('data/tracking-data/goat-calibration-data.csv') %>%
  uere.fit()

d <-
  readRDS('data/tracking-data/all-tracking-data-cleaned-2023-11-17-16-04.rds') %>%
  mutate(
    # convert data frames to telemetry objects
    tel = map(tel, \(x) as.telemetry(x, mark.rm = TRUE)),
    # add calibration-informed error or a reasonable guess
    tel = if_else(species == 'Oreamnos americanus',
                  map(tel, \(x) {
                    uere(x) <- goat_uere
                    return(x)
                  }),
                  map(tel, \(x) {
                    x$error <- 10
                    return(x)
                  })),
    variogram = map(tel, \(x) ctmm.guess(data = x,
                                         CTMM = ctmm(error = TRUE),
                                         interactive = FALSE),
                    .progress = 'Variograms'),
    movement_model = map(1:n(), \(i) ctmm.select(data = tel[[i]],
                                                 CTMM = variogram[[i]]),
                         .progress = 'Movement models'),
    ud = map(1:n(), \(i) akde(data = tel[[i]], CTMM = movement_model,
                              weights = TRUE),
             .progress = 'UDs'))

saveRDS(d, 'models/movement-models.rds')
