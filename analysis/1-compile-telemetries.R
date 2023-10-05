library('dplyr') # for data wrangling (mutate(), %>%, etc.)
library('tidyr') # for data wrangling (nest(), unnest(), pivot_*, etc.)
library('purrr') # for functional programming (map_***(), etc.)
library('ctmm')  # for movement models
source('functions/import_rda.R') # to convert Rda files to rds files

# some duplicates in the elk dataset (~0.02 % of the data)
duplicated_elk <-
  read.csv('data/tracking-data/Elk in southwestern Alberta.csv') %>%
  rename(species = individual.taxon.canonical.name,
         dataset_name = study.name) %>%
  mutate(animal = as.character(individual.local.identifier)) %>%
  group_by(animal) %>%
  filter(duplicated(paste(animal, timestamp)) |
           duplicated(paste(animal, timestamp), fromLast = TRUE))
View(duplicated_elk)
nrow(duplicated_elk) / nrow(read.csv('data/tracking-data/Elk in southwestern Alberta.csv'))

# bind all the telemetry objects into a single tibble
d <-
  bind_rows(
    # mountain goat
    import_rda('data/tracking-data/Oreamnos_americanus.Rda') %>%
      as_tibble() %>% # to print only first 10 rows by default
      select(-X) %>% # remove row name
      mutate(animal = individual.local.identifier, # to add a column of ID
             species = 'Oreamnos_americanus',
             dataset_name = 'Oreamnos_americanus') %>% # some have > 1 data set
      nest(tel = -c(animal, species, dataset_name)) %>%
      mutate(tel = map(tel, as.telemetry)),
    # puma
    import_rda('data/tracking-data/Puma_concolor_2.Rda') %>%
      as_tibble() %>%
      arrange(AnimalID, timestamp) %>% # some data out of order
      mutate(animal = as.character(AnimalID),
             species = 'Puma_concolor',
             dataset_name = 'Puma_concolor_2') %>%
      nest(tel = -c(animal, species, dataset_name)) %>%
      mutate(tel = map(tel, as.telemetry)),
    import_rda('data/tracking-data/Puma_concolor_4.Rda') %>%
      as_tibble() %>%
      mutate(animal = individual.local.identifier,
             species = 'Puma_concolor',
             dataset_name = 'Puma_concolor_4') %>%
      nest(tel = -c(animal, species, dataset_name)) %>%
      mutate(tel = map(tel, as.telemetry)),
    # caribou in south-east BC
    import_rda('data/tracking-data/Rangifer_tarandus.Rda',
               object_name = 'data') %>%
      as_tibble() %>%
      select(- 'Validated') %>% # causes: error in `[<-`:! subscript out of bounds
      mutate(animal = AnimalID,
             species = 'Rangifer_tarandus',
             dataset_name = 'Rangifer_tarandus') %>%
      nest(tel = -c(animal, species, dataset_name)) %>%
      #' using `keep == TRUE` to keep columns of herd and more
      mutate(tel = map(tel, \(x) as.telemetry(x, keep = TRUE))),
    # grizzly
    import_rda('data/tracking-data/Ursus_arctos_horribilis.Rda') %>%
      as_tibble() %>%
      mutate(animal = as.character(AnimalID),
             species = 'Ursus_arctos_horribilis',
             dataset_name = 'Ursus_arctos_horribilis') %>%
      nest(tel = -c(animal, species, dataset_name)) %>%
      mutate(tel = map(tel, as.telemetry)),
    # elk in AB / south-east BC
    # https://datarepository.movebank.org/entities/datapackage/3a171441-6c9b-4bf9-b2fd-0216d962f091
    read.csv('data/tracking-data/Elk in southwestern Alberta.csv') %>%
      rename(species = individual.taxon.canonical.name,
             dataset_name = study.name) %>%
      mutate(animal = as.character(individual.local.identifier)) %>%
      # remove duplicates (too few to make a difference)
      group_by(animal) %>%
      filter(! (duplicated(paste(animal, timestamp)))) %>%
      ungroup() %>%
      nest(tel = -c(animal, species, dataset_name)) %>%
      mutate(tel = map(tel, as.telemetry)),
    # boreal caribou
    readxl::read_xlsx('data/tracking-data/webexportcaribou.xlsx') %>%
      transmute(individual.local.identifier = FieldID,
                location.lat = Latitude,
                location.long = Longitude,
                timestamp = FixDateTime,
                HDOP,
                animal = individual.local.identifier,
                species = 'Rangifer_tarandus',
                dataset_name = 'Rangifer_tarandus_boreal') %>%
      # Status) # collar status (single status for each caribou)
      nest(tel = -c(animal, species, dataset_name)) %>%
      mutate(tel = map(tel, as.telemetry)),
    # boreal wolves
    readxl::read_xlsx('data/tracking-data/webexportwolf.xlsx') %>%
      transmute(individual.local.identifier = FieldID,
                location.lat = Latitude,
                location.long = Longitude,
                timestamp = FixDateTime,
                HDOP,
                animal = individual.local.identifier,
                species = 'Rangifer_tarandus',
                dataset_name = 'Rangifer_tarandus_boreal') %>%
      nest(tel = -c(animal, species, dataset_name)) %>%
      mutate(tel = map(tel, as.telemetry)))

saveRDS(d, 'data/tracking-data/full-dataset.rds')

# print outlier diagnostic plots
N <- nrow(d)
for(i in 1:nrow(d)) {
  cat('Running animal', i, 'of', paste0(N, '.\n'))
  png(filename = paste0('figures/outlier-diagnostics/',
                        stringr::str_replace_all(d$species[i], '_', '-'),
                        '-', d$animal[i], '.png'),
      width = 16, height = 6, units = 'in', res = 300)
  layout(t(1:2))
  out <- outlie(d$tel[[i]], plot = TRUE)
  plot(out, units = FALSE)
  dev.off()
  rm(out)
}
