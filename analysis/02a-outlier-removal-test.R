library('dplyr')     # for data wrangling (mutate(), %>%, etc.)
library('tidyr')     # for data wrangling (nest(), unnest(), pivot_*, etc.)
library('purrr')     # for functional programming (map_***(), etc.)
library('ctmm')      # for movement models
library('lubridate') # for working with dates
library('mapview')   # for interactive maps
library('ggplot2')   # for fancy plots
theme_set(theme_bw())

# source custom functions
source('functions/outlier_plots.R') # to plot outlier diagnostic plots
source('functions/check_animal.R') # to run diagnostic plots
source('functions/plot_adj.R') # to plot 20 adjacent locations
source('functions/flag_outlier.R') # to mark outliers
source('functions/remove_outlier_flags.R') # to start over with an animal

# custom function for finding which values to drop
find_speed_angle <- function(data) {
  # convert to telemetry (quietly)
  tel <- suppressMessages(as.telemetry(data, mark.rm = TRUE))
  # estimate speeds based on error-corrected SLD
  out <- outlie(tel, plot = FALSE)
  # calculate turning angles in degrees
  out$angle <- suppressMessages(find_angle(tel$x, tel$y, radians = FALSE))
  # return the result
  return(out)
}

# import goat data
goat <- readRDS('data/tracking-data/all-tracking-data-not-cleaned.rds') %>%
  filter(animal == '30561') %>%
  select(! c(hdop, animal, species, dataset_name,
             gps.fix.type.raw:gps.satellite.count)) %>%
  mutate(outlier = location.long > 0) # drop locations in germany

outlier_plots(as.telemetry(goat, mark.rm = TRUE)) # still some outliers
hist(filter(goat, location.long < 0)$location.long)
goat <- mutate(goat, outlier = location.long > -119.7 | outlier)

# import and add goat calibration data
goat_uere <-
  as.telemetry('data/tracking-data/goat-calibration-data.csv') %>%
  uere.fit()
summary(goat_uere)
tel <- as.telemetry(goat, mark.rm = TRUE)

# some locations look like clear GPS errors but they have small PDOP values
plot(tel)
outlier_plots(tel)

# estimate average speed for different levels of points removed
d <- expand_grid(max_angle = seq(180, 150, by = -5),
                 max_speed = seq(0, 0.7, by = 0.05)) %>%
  mutate(
    data = map2(max_angle, max_speed, \(.a, .s) {
      out <- find_speed_angle(goat)
      g <- bind_cols(filter(goat, ! outlier), data.frame(out))
      g$outlier <- (g$angle > .a | g$speed > .s |g$outlier)
      return(g)
    }),
    tel = map(data, \(.d) {
      .tel <- as.telemetry(.d, mark.rm = TRUE)
      uere(.tel) <- goat_uere
      return(.tel)
      }),
    guess = map(tel, ~ ctmm.guess(data = .x, interactive = FALSE)),
    model = map2(tel, guess, ~ ctmm.select(data = .x, CTMM = .y)),
    speed = map_dbl(model, \(.m) {
      if(grepl(pattern = 'OUF', summary(.m)$name)) {
        return(summary(.m, units = FALSE)$CI['speed (meters/second)',
                                             'est'])
      } else {
        return(NA_real_)
      }}))
