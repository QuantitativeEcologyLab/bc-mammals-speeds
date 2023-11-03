source('functions/outlier_plots.R')

#' function to run `outlie_plots()` for a given animal
check_animal <- function(id, ...) {
  tel <- filter(d, animal == id) %>%
    pull(tel) %>%
    first() %>%
    as.telemetry(mark.rm = TRUE)
  outlier_plots(tel, return = TRUE, ...)
}
