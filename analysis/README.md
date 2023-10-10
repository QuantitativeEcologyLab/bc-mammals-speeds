This folder contains all `R` scripts used for analyses in this project (except for custom functions, which are in the `functions` folder, and those used to create the manuscript which are in the `writing` folder). Below is a list of the scripts, in the order in which they should be run to replicate the analysis.

# estimate animals' speeds
`1-compile-data.R` merges all telemetry data into a single tibble, which it saves as an rds file.
`2-movement-models.R` fits variograms, movement models.

# obtain (modeled) weather data
`dowload-climate-data.R`
  `check-ecmwfr-raster-dates.R`
	`check-ecmwfr-raster-dates.R`
	`check-missing-rasters.R`
`nc-weather-data-to-rds.R`

# add weather data to tels
`add-speeds-and-weather.R`

# fit models
`weather-regressions.R`

# predictions for the region
`download-dem.R`
`download-climate-projections.R`
>>> **should predict for the area the animals are in only**
> script for figures

# predictions for BC
`download-bc-dem.R`
`download-bc-climate-projections.R`
`bind-climate-change-predictions.R`
  `check-species-ranges.R`
> script for figures
