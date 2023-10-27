library('dplyr')     # for data wrangling (mutate(), %>%, etc.)
library('tidyr')     # for data wrangling (nest(), unnest(), pivot_*, etc.)
library('purrr')     # for functional programming (map_***(), etc.)
library('ctmm')      # for movement models
library('lubridate') # for working with dates
library('mapview')   # for interactive maps
# source custom functions
source('functions/outlier_plots.R') # to plot outlier diagnostic plots
source('functions/check_animal.R') # to run diagnostic plots
source('functions/plot_adj.R') # to plot 20 adjacent locations
source('functions/flag_outlier.R') # to mark outliers
source('functions/remove_outlier_flags.R') # to start over with an animal

# import the full dataset ----
# some NA timestamps (not enough to make a difference)
readRDS('data/tracking-data/all-tracking-data-not-cleaned.rds') %>%
  filter(is.na(timestamp)) %>%
  group_by(dataset_name) %>%
  summarize(n = n())

# should only have one column of HDOP
readRDS('data/tracking-data/all-tracking-data-not-cleaned.rds') %>%
  filter(! is.na(timestamp)) %>%
  mutate(outlier = if_else(outlier, 1, 0)) %>%
  filter(! is.na(hdop) + ! is.na(HDOP) > 1)

# should ony have one column of DOP
readRDS('data/tracking-data/all-tracking-data-not-cleaned.rds') %>%
  filter(! is.na(timestamp)) %>%
  mutate(outlier = if_else(outlier, 1, 0)) %>%
  filter(! is.na(DOP) + ! is.na(gps.dop) > 1)

# import the dataset after dropping NAs
d <- readRDS('data/tracking-data/all-tracking-data-not-cleaned.rds') %>%
  filter(! is.na(timestamp)) %>%
  mutate(hdop = if_else(is.na(hdop), HDOP, hdop), # merge error columns
         dop = if_else(is.na(DOP), gps.dop, DOP),
         original_outliers = outlier,
         outlier = if_else(outlier, 1, 0)) %>% # make numeric
  select(! c(HDOP, DOP, gps.dop)) %>% # remove duplicates
  relocate(dop, .after = pdop) %>%
  nest(tel = ! c(species, dataset_name, animal))

# ensure animal IDs are unique
sum(duplicated(d$animal))

# check which species have DOP values
d %>%
  unnest(tel) %>%
  group_by(species) %>%
  summarise(across(c(dop, hdop, pdop), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(has_dop = dop + hdop + pdop > 0) %>%
  arrange(desc(has_dop))

# 0 == ok, 1 == outlier, 2 == unsure
#' **NOTE:** `as.telemetry()` removes all rows with `outlier > 0`
unique(unnest(d, tel)$outlier)

# print outlier diagnostic plots
if(FALSE) { # initial diagnostic plots
  N <- nrow(d)
  
  for(i in 1:N) {
    cat('Running animal', i, 'of', N, paste0(d$species[i], '.\n'))
    png(filename = paste0('figures/outlier-diagnostics/',
                          stringr::str_replace_all(d$species[i], '_', '-'),
                          '-', d$animal[i], '.png'),
        width = 10, height = 10, units = 'in', res = 300)
    as.telemetry(d$tel[[i]], mark.rm = TRUE) %>%
      outlier_plots()
    dev.off()
  }
}

# Canis lupus (cleaned) ----
# BW008
out <- check_animal('BW008')
plot_adj('BW008', max_speed = 3.5)
plot_adj('BW008', max_angle = 178, max_speed = 1) # using a linear feature
plot_adj('BW008', max_angle = 90, max_speed = 2) # realistic movement
sf::st_as_sf(d$tel[[which(d$animal == 'BW008')]],
             coords = c('location.long', 'location.lat'),
             crs = 4326) %>%
  # view data with a map background
  mapview::mapview()

# BW009
out <- check_animal('BW009')
plot_adj('BW009', max_angle = 175, max_speed = 1, n_adj = 4) # look closer
plot_adj('BW009', max_angle = 179.7, max_speed = 1) # looks bad
plot_adj('BW009', max_angle = 179.7, max_speed = 1, n_adj = 200) # using LF
plot_adj('BW009', max_angle = 175, max_speed = 1, n_adj = 12) # seem ok
plot_adj('BW009', max_angle = 170, min_angle = 175, max_speed = 1,
         n_adj = 2) # linear features
plot_adj('BW009', max_angle = 160, min_angle = 170, max_speed = 1,
         n_adj = 2) # reasonable

# BW010
out <- check_animal('BW010')
plot_adj('BW010', max_speed = 3.5)
plot_adj('BW010', max_speed = 2, max_angle = 135) # seems ok

# BW012 has one likely outlier; dropping it to be safe
out <- check_animal('BW012')
plot_adj('BW012', max_speed = 3)
plot_adj('BW012', max_speed = 2, max_angle = 170)
plot(as.telemetry(d$tel[[which(d$animal == 'BW012')]]), error = FALSE,
     type = 'l', xlim = c(-2600, 0), ylim = c(300, 2200), col = 'black')
plot(as.telemetry(d$tel[[which(d$animal == 'BW012')]])[out$speed > 2 &
                                                         out$angle > 170,],
     error = FALSE, col = 'blue', pch = 19, add = TRUE)
plot(as.telemetry(d$tel[[which(d$animal == 'BW012')]]), add = TRUE)
flag_outlier('BW012', max_speed = 2, max_angle = 170, value = 1)
out <- check_animal('BW012')

# BW014 has a clear GPS error
out <- check_animal('BW014')
plot_adj('BW014', max_speed = 4)
flag_outlier(id = 'BW014', max_speed = 4, value = 1)
out <- check_animal('BW014')
plot_adj('BW014', max_speed = 2)

# B027
out <- check_animal('BW027')
plot_adj('BW027', max_speed = 3, max_angle = 90) # not problematic

# BW028
out <- check_animal('BW028')
plot_adj('BW028', max_speed = 3) # not problematic

# B029
out <- check_animal('BW029')
plot_adj('BW029', max_speed = 2, max_angle = 135) # need a finer scale
plot_adj('BW029', max_speed = 2, max_angle = 135, n_adj = 2) # in DOP range
plot_adj('BW029', max_speed = 2, max_angle = 135, n_adj = 3)

# B031
out <- check_animal('BW031')
plot_adj('BW031', max_speed = 1, max_angle = 135) # need a finer scale
plot_adj('BW031', max_speed = 1, max_angle = 135, n_adj = 1) # in DOP range
plot_adj('BW031', max_speed = 1, max_angle = 135, n_adj = 5) # reasonable

# B033: sharp and fast turns are near linear features
out <- check_animal('BW033')
plot_adj('BW033', max_speed = 1.5, max_angle = 90, n_adj = 5)

# BW042nex
out <- check_animal('BW042nex')
plot_adj('BW042nex', max_speed = 2.5)

# BW044nex
out <- check_animal('BW044nex')
plot_adj('BW044nex', max_speed = 4)

# BW046nex
out <- check_animal('BW046nex')
plot_adj('BW046nex', max_speed = 1, max_angle = 175, min_speed = 3) # ok
plot_adj('BW046nex', max_speed = 1, min_speed = 3, # problematic
         max_angle = 135, min_angle = 175)
i <- which(out$speed > 1 & out$speed < 3 &
             out$angle > 135 & out$angle < 175)
layout(t(0:1))
points(location.lat ~ location.long,
       d$tel[[which(d$animal == 'BW046nex')]][i, ], col = 'blue', cex = 2)
d$tel[[which(d$animal == 'BW046nex')]][i, 'outlier'] <- 1

out <- check_animal('BW046nex')
plot_adj('BW046nex', max_speed = 1, min_speed = 3, max_angle = 135) # ok

# BW049nex
out <- check_animal('BW042nex')
plot_adj('BW042nex', max_speed = 2.5)

# BW051
out <- check_animal('BW051')
plot_adj('BW051', max_speed = 1.5)

# Cervus elaphus (in progress: search for *?) ----
# E002
out <- check_animal('E002')
plot_adj('E002', max_speed = 0.4)
plot_adj('E002', max_angle = 170, max_speed = 0.2)

# E003
out <- check_animal('E003')
plot_adj('E003', max_speed = 0.5)

# E006
out <- check_animal('E006')
plot_adj('E006', max_speed = 0.8) # ok
plot_adj('E006', max_speed = 0.4, max_angle = 170, map = TRUE) # outlier
#' *? dt is much larger: struggling to find a location?*
out[which(out$speed > 0.4 & out$angle > 170) + (-4:4), ]
'hours' %#% c(43282, 7254)
flag_outlier('E006', max_speed = 0.4, max_angle = 170, value = 1)
out <- check_animal('E006')
plot_adj('E006', max_speed = 0.2, max_angle = 170) # hard to say

# E008
out <- check_animal('E008')
plot_adj('E008', max_speed = 0.3)

# E011: last point is outlier (keeping previous points bc many 0 speeds)
out <- check_animal('E011')
plot_adj('E011', max_speed = 0.4, n_adj = 20)
plot_adj('E011', max_speed = 0.4, n_adj = 20, map = TRUE)
which(out$speed > 0.4) == nrow(out)
flag_outlier('E011', max_speed = 0.4, value = 1)
out <- check_animal('E011')
plot_adj('E011', max_speed = 0.25)

# E015 has multiple GPS malfunctions
out <- check_animal('E015')
plot_adj('E015', max_speed = 10)
flag_outlier(id = 'E015', max_speed = 10, value = 1) # surely outliers
out <- check_animal('E015') # some fully stationary points (NaNs; errors)
plot_adj('E015', max_speed = 1, max_angle = 90)
flag_outlier(id = 'E015', max_speed = 1, max_angle = 90, value = 1)
out <- check_animal('E015')
plot_adj('E015', max_speed = 1, reset_layout = FALSE)
# can't filter easily because the outliers don't have the max velocity
i <- d %>%
  pull(tel) %>%
  nth(which(d$animal == 'E015')) %>%
  mutate(i = 1:n()) %>%
  slice(which(timestamp ==
                as.POSIXct(out[which(out$speed > 0.6)[2], 't'])) +
          0:1) %>%
  pull(i)
points(location.lat ~ location.long,
       filter(d, animal == 'E015')$tel[[1]][i, ],
       col = 'blue', cex = 2, lwd = 3)
d$tel[[which(d$animal == 'E015')]][i, 'outlier'] <- 1
out <- check_animal('E015')
plot_adj('E015', max_speed = 0.60) # track looks ok now

# E017
out <- check_animal('E017')
plot_adj('E017', max_speed = 0.6)
plot_adj('E017', max_angle = 170, max_speed = 0.2) #' *? see SW: common*

# E019
out <- check_animal('E019')
plot_adj('E019', max_speed = 0.65, max_angle = 135) # possibly outliers
plot_adj('E019', max_speed = 0.65, max_angle = 160, reset_layout = FALSE)
i <- which(out$speed > 0.65 & out$angle > 160)[c(1, 3)] # some outliers
points(location.lat ~ location.long,
       d$tel[[which(d$animal == 'E019')]][i, ], cex = 2, col = 'blue')
d$tel[[which(d$animal == 'E019')]][i, 'outlier'] <- 1 # no other outliers
out <- check_animal('E019')
plot_adj('E019', max_speed = 0.65, max_angle = 160) # ok
plot_adj('E019', max_speed = 0.6, max_angle = 135, min_angle = 160) # ok

# E026
#' *? CHECK THIS ONE HERE*
#' fitting movement models with different levels of outliers removed to test
#' change in mean speed see (`'functions/test_speeds.R'`)
#' *takes a while to fit the models*
#' does not have an unusual amount of data
hist(map_int(filter(d, species == 'Cervus elaphus')$tel, nrow))
abline(v = nrow(d$tel[[which(d$animal == 'E026')]]), lwd = 2)
d$tel[[which(d$animal == 'E026')]] %>%
  filter(! outlier) %>%
  sf::st_as_sf(coords = c('location.long', 'location.lat'),
               crs = 4326) %>%
  mapview(map.types = 'Esri.WorldImagery', col.regions = 'darkorange',
          col = 'black', lwd = 2)
out <- check_animal('E026') # sharp turns at high speeds
plot_adj('E026', max_speed = 0.6, max_angle = 135)
plot_adj('E026', max_speed = 0.6, max_angle = 135, map = TRUE)
flag_outlier('E026', max_speed = 0.6, max_angle = 135, value = 1)

out <- check_animal('E026')
plot_adj('E026', max_speed = 0.4, max_angle = 135) # not sure of NW one
plot_adj('E026', max_speed = 0.4, max_angle = 135, map = TRUE)
plot_adj('E026', max_speed = 0.42, max_angle = 135) # all likely outliers
flag_outlier('E026', max_speed = 0.42, max_angle = 135, value = 1)

out <- check_animal('E026')
plot_adj('E026', max_speed = 0.35, max_angle = 135) # NW, middle, & NE ok?
plot_adj('E026', max_speed = 0.35, min_speed = 0.4, max_angle = 160,
         reset_layout = FALSE)
i <- with(out, which(speed > 0.35 & speed < 0.4 & angle > 160))[c(2:4)]
points(location.lat ~ location.long,
       filter(d$tel[[which(d$animal == 'E026')]], ! outlier)[i, ],
       cex = 2, col = 'blue', lwd = 2)
#' values in `i` assume outliers have been removed
d$tel[[which(d$animal == 'E026')]][d$tel[[which(d$animal == 'E026')]]$outlier == 0, ][i, 'outlier'] <- 1
# flag_outlier('E026', max_speed = 0.35, max_angle = 160, value = 1)

out <- check_animal('E026')
plot_adj('E026', max_speed = 0.30, max_angle = 160) # these all look odd
flag_outlier('E026', max_speed = 0.30, max_angle = 160, value = 1)

out <- check_animal('E026')
plot_adj('E026', max_speed = 0.25, max_angle = 170, n_adj = 3,
         reset_layout = FALSE) # some outliers
i <- which(out$speed > 0.25 & out$angle > 170)[c(1:3, 5:6)]
points(location.lat ~ location.long,
       d$tel[[which(d$animal == 'E026')]]
       [d$tel[[which(d$animal == 'E026')]]$outlier == 0, ][i, ],
       cex = 2, col = 'blue')
d$tel[[which(d$animal == 'E026')]][d$tel[[which(d$animal == 'E026')]]$outlier == 0, ][i, 'outlier'] <- 1

out <- check_animal('E026') # outliers jump away from ~stationary points
plot_adj('E026', min_speed = 0.25, max_speed = 0.20, max_angle = 170,
         n_adj = 3, reset_layout = FALSE)
i <- which(out$speed > 0.20 & out$speed < 0.25 & out$angle > 170)
points(location.lat ~ location.long,
       d$tel[[which(d$animal == 'E026')]]
       [d$tel[[which(d$animal == 'E026')]]$outlier == 0, ][i, ],
       cex = 2, col = 'blue')
length(i) # check length with figure
d$tel[[which(d$animal == 'E026')]][d$tel[[which(d$animal == 'E026')]]$outlier == 0, ][i, 'outlier'] <- 1

out <- check_animal('E026')
plot_adj('E026', min_speed = 0.20, max_speed = 0.15, max_angle = 170,
         n_adj = 3, reset_layout = FALSE) # likely all outliers
i <- which(out$speed > 0.15 & out$speed < 0.20 & out$angle > 170)
points(location.lat ~ location.long,
       d$tel[[which(d$animal == 'E026')]]
       [d$tel[[which(d$animal == 'E026')]]$outlier == 0, ][i, ],
       cex = 2, col = 'blue')
length(i) # missing point is the first point in the telemetry
d$tel[[which(d$animal == 'E026')]][d$tel[[which(d$animal == 'E026')]]$outlier == 0, ][i, 'outlier'] <- 1

out <- check_animal('E026')
plot_adj('E026', min_speed = 0.15, max_speed = 0.13, max_angle = 170,
         n_adj = 3) # at this point, the movmement is near realistic

# E027
#' *? LIKELY HAS OUTLIERS*
out <- check_animal('E027')
plot_adj('E027', max_speed = 0.3, max_angle = 170, n_adj = 4)
plot_adj('E027', max_speed = 0.3, max_angle = 170, n_adj = 2, map = TRUE)

# E030
#' *? LIKELY HAS OUTLIERS*
out <- check_animal('E030')
plot_adj('E030', max_speed = 0.45)
plot_adj('E030', max_speed = 0.3, min_speed = 0.45)
plot_adj('E030', max_speed = 0.3, min_speed = 0.45, map = TRUE)

# E033
out <- check_animal(id = 'E033')
plot_adj('E033', max_speed = 0.6)

# E034
out <- check_animal(id = 'E034')
plot_adj('E034', max_speed = 0.6)

# E035
out <- check_animal(id = 'E035')
plot_adj('E035', max_speed = 0.6)

# E036 is ok
out <- check_animal(id = 'E036')
plot_adj('E036', max_speed = 0.7) # the next point is along the same line 
layout(t(0:1))
points(location.lat ~ location.long, pch = 19,
       filter(d, animal == 'E036')$tel[[1]][0:1 + which(out$speed > 0.7),])
layout(1)

# E040
out <- check_animal(id = 'E040')
plot_adj('E040', max_speed = 0.6)

# E041
out <- check_animal(id = 'E041')
plot_adj('E041', max_speed = 0.5)

# E044
out <- check_animal(id = 'E044')
plot_adj('E044', max_speed = 0.6)

# E047
out <- check_animal(id = 'E047')
plot_adj('E047', max_speed = 0.8)

# E048
out <- check_animal(id = 'E048')
plot_adj('E048', max_speed = 0.6)

# E052
out <- check_animal(id = 'E052')
plot_adj('E052', max_speed = 0.5)

# E056
out <- check_animal(id = 'E056')
plot_adj('E056', max_speed = 0.5)

# E057
out <- check_animal(id = 'E057')
plot_adj('E057', max_speed = 0.55)

# E061
out <- check_animal(id = 'E061')
plot_adj('E061', max_speed = 0.55)

# E062
out <- check_animal(id = 'E062')
plot_adj('E062', max_speed = 0.5)

# E063
out <- check_animal(id = 'E063')
plot_adj('E063', max_speed = 0.6)

# E064
out <- check_animal(id = 'E064')
plot_adj('E064', max_speed = 0.6)

# E065
out <- check_animal(id = 'E065')
plot_adj('E065', max_speed = 0.4)

# E067
out <- check_animal(id = 'E067')
plot_adj('E067', max_speed = 0.6)

# E068
out <- check_animal(id = 'E068')
plot_adj('E068', max_speed = 0.65)
out[out$speed > 0.65, ] # first point is an outlier (see row names)
layout(t(0:1))
points(location.lat ~ location.long,
       filter(d, animal == 'E068')$tel[[1]][1, ], col = 'blue', pch = 19)
layout(1)
out <- check_animal(id = 'E068') # just to create the plot again
d[d$animal == 'E068', ]$tel[[1]]$outlier[1] <- 1
out <- check_animal(id = 'E068')

# E073
out <- check_animal(id = 'E073')
plot_adj('E073', max_speed = 0.6)

# E074
out <- check_animal(id = 'E074')
plot_adj('E074', max_speed = 0.5)

# E076
out <- check_animal(id = 'E076')
plot_adj('E076', max_speed = 0.4)

# E078
out <- check_animal(id = 'E078')
plot_adj('E078', max_speed = 0.5)

# E079
out <- check_animal(id = 'E079')
plot_adj('E079', max_speed = 0.6)

# E080
out <- check_animal(id = 'E080')
plot_adj('E080', max_speed = 0.8)

# E082
out <- check_animal(id = 'E082')
plot_adj('E082', max_speed = 0.45)

# E083
out <- check_animal(id = 'E083')
plot_adj('E083', max_speed = 0.4)

# E084 is not worth keeping
out <- check_animal(id = 'E084')
plot(location.lat ~ location.long, filter(d, animal == 'E084')$tel[[1]],
     type = 'b', pch = 19, col = '#00000010')
nrow(filter(d, animal == 'E084')$tel[[1]])
range(filter(d, animal == 'E084')$tel[[1]]$timestamp)
d <- filter(d, animal != 'E084')

# E085
out <- check_animal(id = 'E085')
plot_adj('E085', max_speed = 1) # path seems too long
# unlikely that the deer moved so far and so fast
filter(d, animal == 'E085') %>%
  pull(tel) %>%
  first() %>%
  slice(seq(min(which(out$speed > 1)) - 10,
            max(which(out$speed > 1)) + 10)) %>%
  as.telemetry() %>%
  plot()
out <- check_animal(id = 'E085')
flag_outlier(id = 'E085', max_speed = 1, value = 1)
out <- check_animal('E085')
plot_adj('E085', max_speed = 0.6) # seems ok

# E086
out <- check_animal(id = 'E086')
plot_adj('E086', max_speed = 0.6)

# E087
out <- check_animal(id = 'E087')
plot_adj('E087', max_speed = 0.6)

# E088
out <- check_animal(id = 'E088')
plot_adj('E088', max_speed = 0.45)

# E091
out <- check_animal(id = 'E091')
plot_adj('E091', max_speed = 0.5)

# E092
out <- check_animal(id = 'E092')
plot_adj('E092', max_speed = 0.5)

# E096
out <- check_animal(id = 'E096')
plot_adj('E096', max_speed = 0.6)

# E097
out <- check_animal(id = 'E097')
plot_adj('E097', max_speed = 0.5)

# E098
out <- check_animal(id = 'E098')
plot_adj('E098', max_speed = 0.5)

# E101
out <- check_animal(id = 'E101')
plot_adj('E101', max_speed = 0.8)

# E113
out <- check_animal(id = 'E113')
plot_adj('E113', max_speed = 0.5)

# E114
out <- check_animal(id = 'E114')
plot_adj('E114', max_speed = 0.6)

# E115
out <- check_animal(id = 'E115')
plot_adj('E115', max_speed = 0.6)
flag_outlier(id = 'E115', max_speed = 0.6, value = 1)
out <- check_animal('E115')
plot_adj('E115', max_speed = 0.5)

# E116
out <- check_animal(id = 'E116')
plot_adj('E116', max_speed = 0.8)
flag_outlier(id = 'E116', max_speed = 0.8, value = 2)
out <- check_animal('E116')
out[out$speed > 0.68, ]
plot_adj('E116', max_speed = 0.68) # one potential outlier
#' *NOTE:* using `rownames()` only works if `rowname < 999 = max(rowname)`
i <- as.numeric(rownames(out[out$speed > 0.68 & out$distance > 6373, ]))
layout(t(0:1))
points(location.lat ~ location.long,
       filter(d, animal == 'E116') %>%
         pull(tel) %>%
         first() %>%
         filter(! outlier) %>%
         slice(i),
       cex = 2, col = 'blue')
layout(1)
i_tel <- which(as.numeric(filter(d, animal == 'E116')$tel[[1]]$timestamp)
               == out$t[i])
d$tel[[which(d$animal == 'E116')]][i_tel, 'outlier'] <- 1
rm(i_tel, i)

# E117
out <- check_animal(id = 'E117')
plot_adj('E117', max_speed = 0.7) # three outliers
flag_outlier(id = 'E117', max_speed = 0.7, value = 1)
out <- check_animal('E117')
plot_adj('E117', max_speed = 0.6) # maybe just a skittish elk
flag_outlier(id = 'E117', max_speed = 0.6, value = 1)
out <- check_animal('E117')
plot_adj('E117', max_speed = 0.55)
flag_outlier(id = 'E117', max_speed = 0.55, value = 1)
out <- check_animal('E117')
plot_adj('E117', max_speed = 0.4) # too many more to check. seems ok?

# E120
out <- check_animal(id = 'E120')
plot_adj('E120', max_speed = 0.7)

# E121
out <- check_animal(id = 'E121')
plot_adj('E121', max_speed = 0.5)

# E122
out <- check_animal(id = 'E122')
plot_adj('E122', max_speed = 0.5)

# E123
out <- check_animal(id = 'E123')
plot_adj('E123', max_speed = 0.6)

# E127
out <- check_animal(id = 'E127')
plot_adj('E127', max_speed = 0.4)

# E128
out <- check_animal(id = 'E128')
plot_adj('E128', max_speed = 0.5)

# E129
out <- check_animal(id = 'E129')
plot_adj('E129', max_speed = 0.6)

# E130
out <- check_animal(id = 'E130')
plot_adj('E130', max_speed = 0.45)

# E135
out <- check_animal(id = 'E135')
plot_adj('E135', max_speed = 0.7)

# E137
out <- check_animal(id = 'E137')
plot_adj('E137', max_speed = 0.5)

# E138
out <- check_animal(id = 'E138')
plot_adj('E138', max_speed = 0.8)

# E139
out <- check_animal(id = 'E139')
plot_adj('E139', max_speed = 0.6)

# E140
out <- check_animal(id = 'E140')
plot_adj('E140', max_speed = 0.6)
flag_outlier('E140', max_speed = 0.6, value = 2) # speeds are ok
out <- check_animal(id = 'E140')
plot_adj('E140', max_speed = 0.35) # no issues

# saved 'data/tracking-data/all-tracking-data-cleaned-2023-10-14-15-54.rds'
# E141
out <- check_animal(id = 'E141')
plot_adj('E141', max_speed = 0.5)

# E142
out <- check_animal(id = 'E142')
plot_adj('E142', max_speed = 10)
flag_outlier('E142', max_speed = 10, value = 1) # definitely an outlier
out <- check_animal(id = 'E142')
plot_adj('E142', max_speed = 0.8)

# saved 'data/tracking-data/all-tracking-data-cleaned-2023-10-14-15-59.rds'

# E144
out <- check_animal(id = 'E144')
plot_adj('E144', max_speed = 1)

# E146
out <- check_animal(id = 'E146')
plot_adj('E146', max_speed = 0.8)

# E150
out <- check_animal(id = 'E150')
plot_adj('E150', max_speed = 0.6)

# E159
out <- check_animal(id = 'E159')
plot_adj('E159', max_speed = 0.8)

# E160
out <- check_animal(id = 'E160')
plot_adj('E160', max_speed = 0.6)

# E164: first point is definitely an outlier
out <- check_animal(id = 'E164')
tel <- d$tel[[which(d$animal == 'E164')]]
plot(location.lat ~ location.long, tel, type = 'b')
points(location.lat ~ location.long, tel[1, ], col = 'red', pch = 19)
rm(tel)
d$tel[[which(d$animal == 'E164')]]$outlier[1] <- 1
out <- check_animal(id = 'E164')

# E168 has many "bounces", but all are at reasonable speeds
out <- check_animal(id = 'E168')
plot(as.telemetry(d$tel[[which(d$animal == 'E168')]]))
plot_adj('E168', max_speed = 0.8)

# E170
out <- check_animal(id = 'E170')
plot_adj('E170', max_speed = 1)

# E171
out <- check_animal(id = 'E171')
plot_adj('E171', max_speed = 0.6)

# E179
out <- check_animal(id = 'E179')
plot_adj('E179', max_speed = 0.8)

# saved 'data/tracking-data/all-tracking-data-cleaned-2023-10-15-10-21.rds'

# Oreamnos americanus (need to clean) ----
d %>%
  filter(species == 'Oreamnos americanus') %>%
  unnest(tel) %>%
  pull(hdop) %>%
  hist(main = 'Goat HDOP before as.telemetry()')

d %>%
  filter(species == 'Oreamnos americanus') %>%
  mutate(tel = map(tel, \(.t) data.frame(as.telemetry(.t)))) %>%
  unnest(tel) %>%
  pull(HDOP) %>%
  hist(main = 'Goat HDOP after as.telemetry()')

# 30548
# 35 outliers with massive deviation but zero speed
out <- check_animal(id = '30548')
filter(data.frame(out), distance > 2e6)
flag_outlier('30548', max_distance = 2e6, value = 1)
out <- check_animal(id = '30548')
# another 19 outliers with large deviation but zero speed
filter(data.frame(out), distance > 5e4)
flag_outlier('30548', max_distance = 5e4, value = 1)
out <- check_animal(id = '30548')
plot_adj('30548', max_speed = 0.5)
# remaining points are ok within the range of the data
plot_adj('30548', max_speed = 0.1, max_angle = 135, n_adj = 2)

# 30551
# 35 outliers with massive deviation but zero speed
out <- check_animal(id = '30551')
filter(data.frame(out), distance > 2e6)
flag_outlier('30551', max_distance = 2e6, value = 1)
# another 124 outliers, many of which with zero speed
# many of these are at the end of the telemetry
out <- check_animal(id = '30551')
filter(data.frame(out), distance > 2e4)$speed
flag_outlier('30551', max_distance = 2e4, value = 1)
# last point is still an outlier
out <- check_animal(id = '30551', ci_level = 0.95)
out <- check_animal(id = '30551')
which(out$speed > 0.6) == nrow(out)
flag_outlier('30551', max_speed = 0.6, value = 1)
out <- check_animal(id = '30551')
# remaining problematic points are ok within the range if the error
plot_adj('30551', max_speed = 0.3, max_angle = 135)

# 30561
out <- check_animal(id = '30561')
filter(data.frame(out), distance > 2e6)
flag_outlier('30561', max_distance = 2e6, value = 1)
# many more outliers with zero speed
out <- check_animal(id = '30561')
filter(data.frame(out), distance > 5e4)$speed
flag_outlier('30561', max_distance = 5e4, value = 1)
# one more outlier
out <- check_animal(id = '30561')
flag_outlier('30561', max_distance = 15e3, value = 1)
# other smaller outliers, but ok within the range of the error
out <- check_animal(id = '30561')
plot_adj('30561', n_adj = 2, max_speed = 0.3, max_angle = 150)
plot_adj('30561', n_adj = 2, max_speed = 0.25, min_speed = 0.3,
         max_angle = 150)
plot_adj('30561', n_adj = 2, max_speed = 0.25, min_speed = 0.3,
         max_angle = 150)
plot_adj('30561', n_adj = 2, max_speed = 0.2, min_speed = 0.25,
         max_angle = 150)
plot_adj('30561', n_adj = 2, max_speed = 0.1, min_speed = 0.2,
         max_angle = 150)
plot(as.telemetry(d$tel[[which(d$animal == '30561')]], mark.rm = TRUE))

# 30567
out <- check_animal(id = '30567')
filter(data.frame(out), distance > 2e6)
flag_outlier('30567', max_distance = 2e6, value = 1)

out <- check_animal(id = '30567')
flag_outlier('30567', max_distance = 3e4, value = 1)
# some remaining potential outliers, but reasonable based on the error
out <- check_animal(id = '30567')
plot_adj('30567', max_speed = 0.4)

# 30575
# outliers with massive deviation
out <- check_animal(id = '30575')
flag_outlier('30575', max_distance = 2e6, value = 1)
# again more with a large deviation
out <- check_animal(id = '30575')
flag_outlier('30575', max_distance = 3e4, value = 1)
# one more with a relatively smaller (but still large) deviation
out <- check_animal(id = '30575')
plot_adj('30575', max_speed = 1)
flag_outlier('30575', max_speed = 0.8, value = 1)
# still some problematic points with speed speed > 0.5 m/s
out <- check_animal(id = '30575')
plot_adj('30575', max_speed = 0.5, n_adj = 2)
flag_outlier('30575', max_speed = 0.5, value = 1)
# remaining points are ok within the range of the error
out <- check_animal(id = '30575')
plot_adj('30575', max_speed = 0.2, max_angle = 135, n_adj = 2)

# 30599
out <- check_animal(id = '30599')
filter(data.frame(out), distance > 2e6)
flag_outlier('30599', max_distance = 2e6, value = 1)

out <- check_animal(id = '30599')
filter(data.frame(out), distance > 3e4)
flag_outlier('30599', max_distance = 3e4, value = 1)
# some outliers without overlapping errors
out <- check_animal(id = '30599')
plot_adj(id = '30599', max_speed = 0.4)
flag_outlier('30599', max_speed = 0.4, value = 1)
# remaining points seem ok within the range of the error
out <- check_animal(id = '30599')
plot_adj(id = '30599', max_speed = 0.3, max_angle = 160, n_adj = 1)
plot(out, ylim = c(0, 0.5))

# 30613
out <- check_animal(id = '30613')
filter(data.frame(out), distance > 2e6)
flag_outlier('30613', max_distance = 2e6, value = 1)
# some more clear outliers
out <- check_animal(id = '30613')
flag_outlier('30613', max_distance = 3e4, value = 1)
# one more outlier outside the range of the data
out <- check_animal(id = '30613')
plot_adj('30613', max_speed = 0.41, max_angle = 170, n_adj = 1)
flag_outlier('30613', max_speed = 0.41, max_angle = 170, value = 1)
# remaining points are within the range of the errors
out <- check_animal(id = '30613')
plot_adj('30613', max_speed = 0.3, max_angle = 170, n_adj = 2)

# 30636
# some clear sets of outliers
out <- check_animal(id = '30636')
flag_outlier('30636', max_distance = 2e6, value = 1)

out <- check_animal(id = '30636')
flag_outlier('30636', max_distance = 3e4, value = 1)

out <- check_animal(id = '30636')
flag_outlier('30636', max_distance = 15e3, value = 1)
# this outlier is less clear
out <- check_animal(id = '30636')
plot_adj('30636', max_speed = 0.4)
flag_outlier('30636', max_speed = 0.4, value = 1)
# remaining data looks ok
out <- check_animal(id = '30636')
plot_adj('30636', max_speed = 0.25)

# 30642
out <- check_animal(id = '30642')
flag_outlier('30642', max_distance = 2e6, value = 1)
out <- check_animal(id = '30642')
flag_outlier('30642', max_distance = 3e4, value = 1)
out <- check_animal(id = '30642')
plot_adj('30642', max_speed = 1) # outside the error range
flag_outlier('30642', max_speed = 1, value = 1)
# remaining data looks ok given the errors
out <- check_animal(id = '30642')
plot_adj('30642', max_speed = 0.3, max_angle = 135, n_adj = 1)
plot_adj('30642', max_speed = 0.2, min_speed = 0.3, max_angle = 135,
         n_adj = 1)

# 30648
out <- check_animal(id = '30648')
flag_outlier('30648', max_distance = 2e6, value = 1)
out <- check_animal(id = '30648')
flag_outlier('30648', max_distance = 3e4, value = 1)
out <- check_animal(id = '30648')
flag_outlier('30648', max_speed = 1, value = 1)
out <- check_animal(id = '30648')
# remaining points are within the range of the error for speed > 0.4
plot_adj('30648', n_adj = 2, max_speed = 0.4, max_angle = 135)
# except for one, remaining points are within the range of the error
plot_adj('30648', n_adj = 2, max_speed = 0.3, min_speed = 0.4,
         max_angle = 135)
plot_adj('30648', n_adj = 2, max_speed = 0.35, min_speed = 0.36,
         max_angle = 170)
i <- which(out$speed > 0.35 & out$speed < 0.36)
to <- as.telemetry(d$tel[[which(d$animal == '30648')]], mark.rm = TRUE)[i,]
layout(t(0:1))
points(latitude ~ longitude, data.frame(to), col = 'blue', cex = 2)
#' need new index because `tel` also has rows with `outlier = 1`
i_tel <- with(d$tel[[which(d$animal == '30648')]],
              which(round(location.long, 2) == -120.24 &
                      round(location.lat, 2) == 49.02))
d$tel[[which(d$animal == '30648')]]$outlier[i_tel] <- 1
out <- check_animal(id = '30648')
plot_adj('30648', n_adj = 2, max_speed = 0.3, min_speed = 0.4,
         max_angle = 135)

goats <- d %>%
  filter(species == 'Oreamnos americanus') %>%
  unnest(tel) %>%
  select(! c(gps.fix.type.raw, NAV, HDOP, DOP, gps.dop,
             gps.satellite.count))
write.csv(goats, 'data/goat-data.csv')

# Puma concolor (need to clean) ----
# 
filter(d, species == 'Puma concolor') %>%
  pull(tel) %>%
  map_dfr(identity) %>%
  filter(! outlier,
         location.long > -119.8, location.long < -119.2,
         location.lat > 49.7, location.lat < 49.92) %>%
  sf::st_as_sf(coords = c('location.long', 'location.lat'),
               crs = 4326) %>%
  mapview(map.types = 'OpenStreetMap', col.regions = 'darkorange',
          col = 'black', lwd = 2)

# 246 has a clear outlier
out <- check_animal(id = '246')
plot_adj('246', max_speed = 3)
flag_outlier('246', max_speed = 3, value = 1)
# remaining data seem ok
out <- check_animal(id = '246')
plot_adj('246', max_speed = 0.5, n_adj = 3)
plot_adj('246', max_speed = 0.2, n_adj = 3, max_angle = 170)
plot_adj('246', max_speed = 0.2, n_adj = 3, max_angle = 170, map = TRUE)

# 272 has a clear speed outlier, but locations are ok
out <- check_animal(id = '272')
plot_adj('272', max_speed = 1) # data seem ok

# C3 has a clear outlier
out <- check_animal(id = 'C3')
plot_adj('C3', max_speed = 1)
plot_adj('C3', max_speed = 1, map = TRUE)
flag_outlier('C3', max_speed = 1, value = 1)
out <- check_animal(id = 'C3') # remaining data are ok
plot_adj('C3', max_speed = 0.4, max_angle = 170, n_adj = 5) # ok
plot_adj('C3', max_speed = 0.4, max_angle = 170, n_adj = 5, map = TRUE)

# C4
out <- check_animal(id = 'C4')
plot_adj('C4', max_speed = 0.64)
plot_adj('C4', max_speed = 0.64, map = TRUE)
flag_outlier('C4', max_speed = 0.64, value = 1) # clear outlier (DOP = 1)
out <- check_animal(id = 'C4') # remaining data are ok
plot_adj('C4', max_speed = 0.6)
plot_adj('C4', max_speed = 0.6, map = TRUE)

# C5
out <- check_animal(id = 'C5')
plot_adj('C5', max_speed = 0.55) # ok
plot_adj('C5', max_speed = 0.4, max_angle = 170) # ok

# C6
#' **HERE**
out <- check_animal(id = 'C6')
plot_adj('C6', max_speed = 0.47, reset_layout = FALSE)
# droping sothern point because DOP is not provided
tel <- d$tel[[which(d$animal == 'C6')]]
i <- which(out$speed > 0.47)[1]
points(location.lat ~ location.long, tel[i, ], col = 'blue', cex = 2)
d$tel[[which(d$animal == 'C6')]][i, 'outlier'] <- 1
out <- check_animal(id = 'C6')
plot_adj('C6', max_speed = 0.47) # check correct removal
plot_adj('C6', max_speed = 0.1, max_angle = 170) #' *? some outliers, some ok*
rm(tel, i)

#' C08 *?looks like it ran and then returned to where it was before*
out <- check_animal(id = 'C8')
plot_adj('C8', max_speed = 0.55)
plot_adj('C8', max_speed = 0.55, map = TRUE)

# C11
out <- check_animal(id = 'C11')
plot_adj('C11', max_speed = 2)
flag_outlier('C11', max_speed = 2, value = 1)
# remaining data seem ok
out <- check_animal(id = 'C11')
plot_adj('C11', max_speed = 0.8)
plot_adj('C11', max_speed = 0.4, max_angle = 170) # NE ok
plot_adj('C11', max_speed = 0.4, max_angle = 175, map = TRUE) #' *? outlier?*
out[which(out$speed > 0.4 & out$angle > 175) + -3:3, ] # regular sampling

# C12 has two points that look like consecutive errors
#' *HERE*
out <- check_animal(id = 'C12')
plot(out)
plot_adj('C12', max_speed = 0.8)
i <- which(out$speed > 0.8) + c(-1:0)
layout(t(0:1))
points(location.lat ~ location.long, d$tel[[which(d$animal == 'C12')]][i,],
       cex = 2, col = 'blue')
d$tel[[which(d$animal == 'C12')]][i, 'outlier'] <- 2
out <- check_animal(id = 'C12')
plot_adj(id = 'C12', max_speed = 0.6) # remaining data are ok
rm(i)

# saved 'data/tracking-data/all-tracking-data-cleaned-2023-10-15-15-03.rds'

# C16
out <- check_animal('C16')
plot_adj('C16', max_speed = 0.6)

# C17
out <- check_animal('C17')
plot_adj('C17', max_speed = 0.6)

# C18
out <- check_animal('C18')
plot_adj('C18', max_speed = 0.6)
flag_outlier('C18', max_speed = 0.6, value = 2)
out <- check_animal('C18')
plot_adj('C18', max_speed = 0.45)

# C29
out <- check_animal('C29')
i <- which(out$speed > 0.5)[1]
plot_adj('C29', max_speed = 0.5)
layout(t(0:1))
points(location.lat ~ location.long,
       filter(d, animal == 'C29')$tel[[1]][i, ], col = 'blue', cex = 2)
layout(1)
d$tel[[which(d$animal == 'C29')]][i, 'outlier'] <- 2
plot(location.lat ~ location.long, d$tel[[which(d$animal == 'C29')]],
     col = factor(d$tel[[which(d$animal == 'C29')]]$outlier), pch = 19)

# C30
out <- check_animal('C30')
with(out, plot((t - lag(t)) / (1 %#% 'hour'), speed, xlim = c(0, 5),
               col = factor(speed > 0.6), pch = 19))
plot_adj('C30', max_speed = 0.6)
layout(t(0:1))
i <- which(out$speed > 0.6) + 0:1
points(location.lat ~ location.long,
       filter(d, animal == 'C30')$tel[[1]][i, ], col = 'blue', cex = 2)
d$tel[[which(d$animal == 'C30')]][i, 'outlier'] <- 2

plot(location.lat ~ location.long, d$tel[[which(d$animal == 'C30')]],
     col = factor(d$tel[[which(d$animal == 'C30')]]$outlier), pch = 19)

# C31
out <- check_animal('C31')
plot_adj('C31', max_speed = 0.37)
layout(t(0:1))
i <- which(out$speed > 0.37)[2]
points(location.lat ~ location.long,
       filter(d, animal == 'C31')$tel[[1]][i, ], col = 'blue', cex = 2)
layout(1)
d$tel[[which(d$animal == 'C31')]][i, 'outlier'] <- 2

plot(location.lat ~ location.long, d$tel[[which(d$animal == 'C31')]],
     col = factor(d$tel[[which(d$animal == 'C31')]]$outlier), pch = 19)

# saved 'data/tracking-data/all-tracking-data-cleaned-2023-10-16-08-37.rds'

# Mountain caribou (need to clean) ----
# 92_prepenned
ID <- '92_prepenned'
out <- check_animal(ID)
plot_adj(ID, max_speed = 0.35)

# 101_prepenned
ID <- '101_prepenned'
out <- check_animal(ID)
plot_adj(ID, max_speed = 0.5)
which(out$speed > 0.5)
d$tel[[which(d$animal == ID)]][1, 'outlier'] <- 1 # ran after collaring
out <- check_animal(ID)
plot_adj(ID, max_speed = 0.2, max_angle = 170) # ok
plot_adj(ID, max_speed = 0.1, min_speed = 0.2, max_angle = 170) # ok

# 104_prepenned
ID <- '104_prepenned'
out <- check_animal(ID)
plot_adj(ID, max_speed = 0.4)


# Ursus arctos horribilis ----
# 197
out <- check_animal('197')
plot_adj('197', max_speed = 0.3, max_angle = 170, n_adj = 3)

# 242: first point is an outlier
out <- check_animal('242')
plot_adj('242', max_speed = 1, n_adj = 10)
which(out$speed > 1)
d$tel[[which(d$animal == '242')]][1, 'outlier'] <- 1
out <- check_animal('242')

# 259
out <- check_animal('259')
plot_adj('259', max_speed = 0.4, max_angle = 170, n_adj = 10)
flag_outlier('259', max_speed = 0.4, max_angle = 170, value = 1)
out <- check_animal('259')
plot_adj('259', max_speed = 0.2, max_angle = 135, n_adj = 5) # ok

# 260
out <- check_animal('260')
plot_adj('260', max_speed = 1, max_angle = 170, n_adj = 10)
flag_outlier('260', max_speed = 1, max_angle = 170, value = 1)
out <- check_animal('260')
plot_adj('260', max_speed = 0.4, max_angle = 170, n_adj = 10) # ok
plot_adj('260', max_speed = 0.3, max_angle = 170, n_adj = 10, # unsure
         min_speed = 0.4)
filter(data.frame(out), round(speed, 1) == 0.4 & angle > 170)
0.3509840 * 3645 # reasonable distance in an hour

# 262
out <- check_animal('262')
plot_adj('262', max_speed = 0.3, max_angle = 170, n_adj = 10)

# 291
out <- check_animal('291')
plot_adj('291', max_speed = 0.3, max_angle = 170, n_adj = 10)

# 292
out <- check_animal('292')
plot_adj('292', max_speed = 1.2, n_adj = 10)
flag_outlier(id = '292', max_speed = 1.2, value = 1)
out <- check_animal('292')
plot_adj('292', max_speed = 0.8, n_adj = 10) #' *moving near a road?*

# 297
out <- check_animal('297')
plot_adj('297', max_speed = 0.5) # ok

# 317
out <- check_animal('317')
plot_adj('317', max_speed = 1.5, reset_layout = FALSE) # previous = outlier
i <- which(out$speed > 1.5) - 1
points(location.lat ~ location.long, d$tel[[which(d$animal == '317')]][i, ],
       cex = 2, col = 'blue')
d$tel[[which(d$animal == '317')]][i, 'outlier'] <- 1
out <- check_animal('317')
plot_adj('317', max_speed = 0.8) # ok
plot_adj('317', max_speed = 0.3, max_angle = 170) # possible outliers...
plot_adj('317', max_speed = 0.3, max_angle = 160) # but common at 0.3 m/s

# 318
out <- check_animal('318')
plot_adj('318', max_speed = 0.5) # ok
plot_adj('318', max_speed = 0.4, min_speed = 0.5) # ok

# 320
out <- check_animal('320')
plot_adj('320', max_speed = 0.3) # ok
plot_adj('320', max_speed = 0.25) # ok

# to save progress (cleaned) ----
saveRDS(object = d,
        file = paste0('data/tracking-data/all-tracking-data-cleaned-',
                      format(Sys.time(), '%Y-%m-%d-%H-%M'),
                      '.rds'))
