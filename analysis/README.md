This folder contains all `R` scripts used for analyses in this project (except for custom functions, which are in the `functions` folder, and those used to create the manuscript which are in the `writing` folder). Below is a list of the scripts, in the order in which they should be run to replicate the analysis.

## Estimate animals' speeds

* `01-compile-data.R` merges all telemetry data into a single tibble, which it saves as an rds file.
* `02-remove-outliers.R` screens the telemetry data for outliers and removes any problematic locations that the error model could not account for (given the DOP values and estimated error. Since I only have calibration data for the goats, I assume the error to be 10 m for all other telemetries).
* `03-movement-models.R` fits variograms, continuous-time movement models, and utilization distributions.

## Obtain estimated historical weather data

* `04-dowload-climate-data.R` downloads estimated historical hourly weather data from the European Centre for Medium-Range Weather Forecasts
  * `04a-check-ecmwfr-raster-dates.R`
  * `04b-check-ecmwfr-raster-dates.R`
  * `04c-check-missing-rasters.R`
* `05-nc-weather-data-to-rds.R`

## Annotate telelemetry data with hourly weather data

* `06-add-speeds-and-weather.R`

## Estimate effects of weather on amimal movment

* `07-weather-regressions.R`

> scripts for figures

## Make predictions for each species' region

* `08-download-bc-dem.R` **crop for each area to download the data?**
* `09-download-climate-projections.R` **should predict for the dataset range only**
* `10-simulated-daily-weather.R` **to simulate daily weather using mean and extremes. test using hourly historical data.**

> scripts for figures

## Make predictions for BC until 2100

* `11-download-bc-climateNA-data.R`
* `12-bind-climateNA-data.R`
  * `check-species-ranges.R`
  
> scripts for figures
