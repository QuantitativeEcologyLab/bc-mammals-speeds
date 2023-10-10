library('dplyr')     # for data wrangling (mutate(), %>%, etc.)
library('tidyr')     # for data wrangling (nest(), unnest(), pivot_*, etc.)
library('purrr')     # for functional programming (map_***(), etc.)
library('ctmm')      # for movement models
library('lubridate') # for working with dates
source('functions/outlier_plots.R') # for outlier diagnostic plots

# import the full dataset
# some NA timestamps (not enough to make a difference)
readRDS('data/tracking-data/all-tracking-data-not-cleaned.rds') %>%
  filter(is.na(timestamp)) %>%
  group_by(dataset_name) %>%
  summarize(n = n())

# some NA locations
readRDS('data/tracking-data/all-tracking-data-not-cleaned.rds') %>%
  filter(is.na(location.long) | is.na(location.lat)) %>%
  group_by(dataset_name) %>%
  summarize(n = n())

# import the dataset after dropping NAs
d <- readRDS('data/tracking-data/all-tracking-data-not-cleaned.rds') %>%
  filter(! is.na(timestamp)) %>%
  filter(! (is.na(location.long) | is.na(location.lat))) %>%
  nest(tel = ! c(species, dataset_name, animal))

# print outlier diagnostic plots
N <- nrow(d)

if(FALSE) { # initial diagnostic plots
  for(i in 1:N) {
    cat('Running animal', i, 'of', N, paste0(d$species[i], '.\n'))
    png(filename = paste0('figures/outlier-diagnostics/',
                          stringr::str_replace_all(d$species[i], '_', '-'),
                          '-', d$animal[i], '.png'),
        width = 16, height = 6, units = 'in', res = 300)
    as.telemetry(d$tel[[i]], mark.rm = TRUE) %>%
      outlier_plots()
    dev.off()
  }
}

# remove outliers ----
#' function to run `outlie_plots()` for a given animal
check_animal <- function(id, out = NULL, max_speed = NULL,
                         max_distance = NULL) {
  tel <- filter(d, animal == id) %>%
    pull(tel) %>%
    first() %>%
    as.telemetry(mark.rm = TRUE)
  if(! is.null(max_speed)) tel <- tel[out$speed < max_speed, ]
  if(! is.null(max_distance)) tel <- tel[out$distance < max_distance, ]
  outlier_plots(tel, return = TRUE)
}

# function to plot telemetry points near problematic points
plot_adj <- function(id, max_speed = Inf, max_distance = Inf) {
  tel <- filter(d, animal == id)$tel[[1]]
  
  problematic <- which(out$speed >= max_speed |
                         out$distance > max_distance)
  
  adj <- sapply(problematic, \(x) (x-10):(x+10)) %>%
    as.numeric()
  adj <- adj[adj >= 1 & adj <= nrow(tel)]
  
  plot(location.lat ~ location.long, data.frame(tel)[adj, ], type = 'b')
  points(location.lat ~ location.long, data.frame(tel)[problematic, ],
  col = 'red', pch = 19)
}

# Canis lupus ----
# BW008 is ok
out <- check_animal('BW008')
plot_adj('BW008', max_speed = 3.5)

# BW010 is ok
out <- check_animal('BW010')
plot_adj('BW010', max_speed = 3.5)

# BW012 is ok
out <- check_animal('BW012')
plot_adj('BW012', max_speed = 3)

# BW014 has a GPS error
out <- check_animal('BW014')
plot_adj('BW014', max_speed = 4)
d$tel[d$animal == 'BW014'][[1]][out$speed > 4, 'outlier'] <- TRUE
out <- check_animal('BW014')
plot_adj('BW014', max_speed = 2)
saveRDS(object = d,
        file = paste0('data/tracking-data/all-tracking-data-cleaned',
                      format(Sys.time(), '%Y-%m-%d-%H-%M'),
                      '.rds'))

# BW042nex
out <- check_animal('BW042nex')
plot_adj('BW042nex', max_speed = 2.5)

# BW044nex
out <- check_animal('BW044nex')
plot_adj('BW044nex', max_speed = 4)

# BW049nex
out <- check_animal('BW042nex')
plot_adj('BW042nex', max_speed = 2.5)

# BW051
out <- check_animal('BW051')
plot_adj('BW051', max_speed = 1.5)

# Cervus elaphus ----
# E006
out <- check_animal('E006')
plot_adj('E006', max_speed = 0.8)

# E008 is not problematic
out <- check_animal('E008')
plot_adj('E008', max_speed = 0.3)

# E011
out <- check_animal('E011')
plot_adj('E011', max_speed = 0.4)

# E015
out <- check_animal('E015')
# need to split the telemetry into 
plot_adj('E015', max_speed = 10)

out <- check_animal('E015', out = out, max_speed = 2)


# E017?
# E026
# E033?
# E035
# E036?
# E041?
# E047?
# E048?
# E056
# E057
# E061
# E062
# E063
# E064?
# E065?
# E067
# E068?
# E073
# E074
# E076
# E078
# E079?
# E080
# E082
# E083?
# E084
# E085
# E086?
# E089
# E091
# E092
# E096
# E097
# E098
# E100
# E101?
# E113?
# E114
# E115?
# E116
# E117
# E123
# E127?
# E128
# E129?
# E135?
# E137
# E138?
# E139
# E140
# E141
# E142
# E144?
# E150
# E153
# E154
# E156
# E158
# E160
# E164?
# E168
# E170?
# E171?
# E179
# 
# Oreamnos americanus
# GID03
# GID04
# GID05
# GID08?
# GID10?? (unlikely)
# 
# Puma concolor
# 246
# 272
# C3
# C4?
# C6?
# C11
# C12?
# C17?
# C18?
# C29?
# C30?
# C31?
# 
# Mountain caribou
# 
# 
# 
# 
# 
