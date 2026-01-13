suppressPackageStartupMessages({
  library(readr)
  library(ncdf4)
})

grsofun_save_nc <- function(settings) {
  vars <- names(settings$save)[unlist(settings$save) == "mon"]
  stopifnot(length(vars) > 0)

  files <- list.files(
    settings$dir_out,
    pattern = paste0("^", settings$fileprefix, "_mon_LON_.*\\.rds$"),
    full.names = TRUE
  )
  stopifnot(length(files) > 0)

  g <- settings$grid
  nlon <- g$len_ilon
  nlat <- g$len_ilat
  lon  <- g$lon_start + (0:(nlon - 1)) * g$dlon
  lat  <- g$lat_start + (0:(nlat - 1)) * g$dlat

  dir.create(settings$dir_out_nc,
             recursive = TRUE,
             showWarnings = FALSE)
  fill <- 1e20

  dim_lon <- ncdim_def("lon", "degrees_east", lon)
  dim_lat <- ncdim_def("lat", "degrees_north", lat)

  for (year in settings$year_start:settings$year_end) {
    arr <- lapply(vars, \(.) array(fill, c(nlon, nlat, 12)))
    names(arr) <- vars

    for (f in files) {
      x <- readr::read_rds(f)
      if (!nrow(x)) {
        next
      }
      x <- x[x$year == year, , drop = FALSE]
      if (!nrow(x)) {
        next
      }

      miss <- setdiff(c("lon", "lat", "month", vars), names(x))
      if (length(miss))
        stop("Missing in ", basename(f), ": ", paste(miss, collapse = ", "))

      i <- as.integer(round((x$lon[1] - g$lon_start) / g$dlon) + 1L)
      if (i < 1L || i > nlon) {
        next
      }

      j <- as.integer(round((x$lat - g$lat_start) / g$dlat) + 1L)
      m <- as.integer(x$month)
      ok <- !is.na(j) & j >= 1L & j <= nlat & m >= 1L & m <= 12L
      if (!any(ok)) {
        next
      }
      j <- j[ok]
      m <- m[ok]

      for (v in vars) {
        val <- x[[v]][ok]
        keep <- !is.na(val)
        if (any(keep))
          arr[[v]][i, j[keep], m[keep]] <- val[keep]
      }
    }

    time_dates <- as.Date(sprintf("%04d-%02d-01", year, 1:12))
    time_vals  <- as.numeric(time_dates - as.Date("1900-01-01"))
    dim_time   <- ncdim_def("time", "days since 1900-01-01", time_vals, unlim = TRUE)

    var_def <- function(name)
      ncvar_def(name,
                "",
                list(dim_lon, dim_lat, dim_time),
                missval = fill,
                prec = "float")

    nc_path <- file.path(settings$dir_out_nc,
                         sprintf("%s_mon_%04d.nc", settings$fileprefix, year))
    nc <- nc_create(nc_path, lapply(vars, var_def), force_v4 = TRUE)

    for (v in vars)
      ncvar_put(nc, v, arr[[v]])
    nc_close(nc)

    message("Wrote: ", nc_path)
  }

  invisible(TRUE)
}
