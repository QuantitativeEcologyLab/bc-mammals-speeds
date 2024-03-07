library('dplyr')     # for data wrangling
library('lubridate') # for smother date wrangling
library('mgcv')      # for Generalized Additive Models
library('ggplot2')   # for fancy plots
library('khroma')    # for colorblind-friendly color palettes
source('analysis/figures/default-ggplot-theme.R') # bold text and no grids

d <-
  readRDS('../bc-mammals-speeds/data/standardized-speeds-2024-03-05.rds') %>%
  filter(dataset_name == 'Rangifer_tarandus_boreal') %>%
  filter(! is.na(speed_est)) %>% # drop NA speeds
  mutate(animal = factor(animal),
         species = factor(species),
         moving = speed_est > 1e-2, # assume not moving if speed < 1 cm/s
         timestamp_bc = with_tz(timestamp, tz = 'America/Vancouver'),
         time_of_day = (hour(timestamp_bc) +
                          minute(timestamp_bc) / 60 +
                          second(timestamp_bc) / 60 / 60),
         doy = yday(timestamp_bc))

if(is.infinite(max(d$speed_high, na.rm = TRUE))) {
  warning(paste(sum(is.infinite(d$speed_high)),
                'infinite upper limit(s) for speed!'))
}

# add weights (dropping speed estimate with infinite upper limit)
d <- d %>%
  filter(is.finite(speed_high)) %>%
  group_by(animal) %>%
  mutate(speed_weight = 1 / sqrt(speed_high - speed_low),
         speed_weight = speed_weight / mean(speed_weight)) %>%
  ungroup()

# # drop values with NA weather values (6 rasters fail to download)
# d %>%
#   filter(is.na(temperature)) %>%
#   pull(timestamp) %>%
#   round('hours') %>%
#   unique()
# 
# d <- filter(d, ! is.na(temperature))

# drop unrealistic temperatures
range(d$temperature)
d <- filter(d, temperature < 40)

# model P(movement) ----
# not including REs of movment because sampling 
m_1 <-
  bam(moving ~
        # to account for differences between animals
        s(animal, bs = 're') +
        # to account for changes in behavior within days
        s(time_of_day, k = 5, bs = 'cc') +
        # to account for changes in behavior within years
        s(doy, k = 10, bs = 'cc') +
        # effect of temperature
        s(temperature, k = 5, bs = 'fs') +
        # sun rises at different times throughout the year
        ti(doy, time_of_day, bs = c('cc', 'cc'), k = c(5, 10)) +
        # +5 degrees C in winter is different from +5 in summer
        ti(doy, temperature, bs = c('cc', 'cr'), k = c(5, 5)),
      family = binomial(link = 'logit'),
      data = d,
      method = 'fREML', # fast REML
      discrete = TRUE,  # discretize the posterior for faster computation
      select = TRUE, # perform model shrinkage
      knots = list(time_of_day = c(0, 1), # for bs = 'cc'
                   doy = c(0.5, 366.5)))

plot(m_1, pages = 1, scheme = c(0, 1, 1, 1, 3, 3), scale = 0)

saveRDS(m_1, paste0('models/binomial-gam-', Sys.Date(),
                    '-boreal-caribou.rds'))

if(FALSE) {
  layout(matrix(1:4, ncol = 2))
  gam.check(m_1, type = 'pearson')
  layout(1)
}

summary(m_1)

# model mean speed given that animals are moving ----
d_2 <- filter(d, moving)

m_2 <-
  bam(speed_est ~
        # to account for differences between animals
        s(animal, bs = 're') +
        # to account for changes in behavior within days
        s(time_of_day, k = 5, bs = 'cc') +
        # to account for changes in behavior within years
        s(doy, k = 10, bs = 'cc') +
        # effect of temperature
        s(temperature, k = 5, bs = 'fs') +
        # sun rises at different times throughout the year
        ti(doy, time_of_day, bs = c('cc', 'cc'), k = c(5, 5)) +
        # +5 degrees C in winter is different from +5 in summer
        ti(doy, temperature, bs = c('cc', 'cr'), k = c(5, 5)),
      family = Gamma(link = 'log'), # can use Gamma because no zeros
      weights = speed_weight, # account for speed uncertainty
      data = d_2,
      method = 'fREML', # fast REML
      discrete = TRUE,  # discretize the posterior for faster computation
      knots = list(time_of_day = c(0, 1), doy = c(0.5, 366.5)))

plot(m_2, pages = 1, scheme = c(0, 1, 1, 1, 3, 3))

saveRDS(m_2, paste0('models/gamma-gam-', Sys.Date(),
                    '-boreal-caribou.rds'))

if(FALSE) {
  layout(matrix(1:4, ncol = 2))
  gam.check(m_2)
  layout(1)
}

summary(m_2)
