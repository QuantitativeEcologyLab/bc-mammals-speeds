source('functions/outlier_plots.R')

#' function to run `outlie_plots()` for a given animal
check_animal <- function(id, out = NULL, ci_level = 0, ...) {
  tel <- filter(d, animal == id) %>%
    pull(tel) %>%
    first() %>%
    as.telemetry(mark.rm = TRUE)
  outlier_plots(tel, return = TRUE, ci_level = ci_level, ...)
}
