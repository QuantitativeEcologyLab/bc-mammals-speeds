This folder contains all `R` scripts used for analyses in this project (except for custom functions, which are in the `functions` folder, and those used to create the manuscript which are in the `writing` folder). Below is a list of the scripts, in the order in which they should be run to replicate the analysis.

# estimate animals' speeds
`01-compile-data.R` merges all telemetry data into a single tibble, which it saves as an rds file.
`02-remove-outliers.R` screens the telemetry data for outliers and remove any problematic locations that the error model could not account for (given the DOP values and estimated error, which I assume to be 10 m since I don't have calibration data for each collar).
`03-movement-models.R` fits variograms and movement models.

# obtain (modeled) weather data
`04-dowload-climate-data.R`
  `04-a-check-ecmwfr-raster-dates.R`
  `04-b-check-ecmwfr-raster-dates.R`
  `04-c-check-missing-rasters.R`
`05-nc-weather-data-to-rds.R`

# add weather data to tels
`06-add-speeds-and-weather.R`

# fit models
`07-weather-regressions.R`

# predictions for the region
`08-download-bc-dem.R` **crop for each area to download the data**
`09-download-climate-projections.R` **should predict for the area the animals are in only**
> scripts for figures

# predictions for BC
`10-download-bc-climate-projections.R`
`11-bind-climate-change-predictions.R`
  `check-species-ranges.R`
> scripts for figures
