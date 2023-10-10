outlier_plots <- function(telemetry, units = FALSE, ci_level = 0,
                          return = FALSE) {
  layout(t(1:2))
  # plot of locations and velocities
  out <- outlie(telemetry, plot = TRUE)
  # min speed vs distance from tel center (SI units; no CIs)
  plot(out, units = units, level = ci_level)
  layout(1)
  if(return) return(out)
}
