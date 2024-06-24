#' @export
grsofun <- function(par, settings){

  # Convert forcing files to a tidy format
  # Checks by file. If tidy already, skips automatically
  settings <- grsofun_tidy(settings)

  # Run rsofun
  # Parallelizes runs to chunks of longitudinal bands
  error <- grsofun_run(par, settings)

  # Collect output
  # Only variables and at aggregation level required for NetCDF output
  df <- grsofun_collect(settings, return_data = TRUE)

  # Write to NetCDF files - XXX not yet implemented
  error <- grsofun_save_nc(df, settings)

}
