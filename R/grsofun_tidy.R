#' Tidies necessary input data to prepare grsofun_run()
#'
#' @param setting A list of grsofun settings.
#' @param ... Additional arguments that are passed to map2tidy for ALL processed
#'            input types. The documentation of the allowed arguments can be
#'            accessed with `?map2tidy`
#'
#' @return Returns a data.frame with the filenames of the rds files created and
#'         a status message for each file.
#' @export
#'

grsofun_tidy <- function(settings, ...){

  ## Land mask and elevation in one ------------------------------------------
  res_landmask <- if (!is.na(settings$file_in_landmask) && file.exists(settings$file_in_landmask)) {
    map2tidy::map2tidy(
      nclist = settings$file_in_landmask,
      varnam = "elevation",
      lonnam = "lon",
      latnam = "lat",
      do_chunks = TRUE,
      outdir = settings$dir_out_tidy_landmask,
      fileprefix = "WFDEI-elevation",
      overwrite = settings$overwrite,
      ncores = settings$ncores_max,  # parallel::detectCores()
      ...
    )
  } else {
    data.frame(input_path = settings$file_in_landmask, msg = "No landmask file found.")
  }

  ## Land area fraction per gridcell  ------------------------------------------
  res_gicew <- if (!is.na(settings$file_in_gicew) && file.exists(settings$file_in_gicew)) {
    map2tidy::map2tidy(
      nclist = settings$file_in_gicew,
      varnam = "GICEW",
      lonnam = "LON",
      latnam = "LAT",
      do_chunks = TRUE,
      outdir = settings$dir_out_tidy_gicew,
      fileprefix = "gicew",
      overwrite = settings$overwrite,
      ncores = settings$ncores_max,  # parallel::detectCores()
      ...
    )
  } else {
    data.frame(input_path = settings$file_in_gicew, msg = "No land area fracction file found.")
  }

  ## Root zone water storage capacity ----------------------------------------
  res_whc <- if (!is.na(settings$file_in_whc) && file.exists(settings$file_in_whc)) {
    map2tidy(
      nclist = settings$file_in_whc,
      varnam = "whc_2m", #"cwdx80_forcing",
      lonnam = "lon",
      latnam = "lat",
      do_chunks = TRUE,
      outdir = settings$dir_out_tidy_whc,
      fileprefix = gsub(".nc","",basename(settings$file_in_whc)),#"cwdx80_forcing_halfdeg",
      overwrite = settings$overwrite,
      ncores     = settings$ncores_max,  # parallel::detectCores()
      ...
    )
  } else {
    data.frame(input_path = settings$file_in_whc, msg = "No whc file found.")
  }

  ## Elevation ---------------------------------------------------------------
  res_elv <- if (!is.na(settings$file_in_elv) && file.exists(settings$file_in_elv)) {
    map2tidy::map2tidy(
      nclist = settings$file_in_elv,
      varnam = "elevation", # varnam = "ETOPO1_Bed_g_geotiff",
      lonnam = "lon",
      latnam = "lat",
      do_chunks = TRUE,
      outdir = settings$dir_out_tidy_elv,
      fileprefix = "WFDEI-elevation", # fileprefix = "ETOPO1_Bed_g_geotiff_halfdeg",
      overwrite = settings$overwrite,
      ncores    = settings$ncores_max,  # parallel::detectCores()
      ...
    )
  } else {
    data.frame(input_path = settings$file_in_elv, msg = "No elv file found.")
  }

  ## Canopy height -----------------------------------------------------------
  res_canopy <- if (!is.na(settings$file_in_canopy) && file.exists(settings$file_in_canopy)) {
    map2tidy::map2tidy(
      nclist = settings$file_in_canopy,
      varnam = "Band1",
      lonnam = "lon",
      latnam = "lat",
      do_chunks = TRUE,
      outdir = settings$dir_out_tidy_canopy,
      fileprefix = "canopy_height",
      overwrite = settings$overwrite,
      ncores    = settings$ncores_max,  # parallel::detectCores()
      ...
    )
  } else {
    data.frame(input_path = settings$file_in_canopy, msg = "No canopy file found.")
  }

  ## Surface net solar radiation ----------------------------------------------------
  res_ssr <- if (!is.na(settings$dir_in_ssr) &&
                 dir.exists(settings$dir_in_ssr)) {
    ssr_files <- list.files(
      settings$dir_in_ssr,
      recursive = TRUE,
      pattern   = "ERA5Land_UTCDaily\\.tot_ssr\\.[0-9]{4}_halfdeg\\.nc$",
      full.names = TRUE
    )
    stopifnot(length(ssr_files) > 0)
    map2tidy::map2tidy(
      nclist     = ssr_files,
      varnam     = "tot_ssr",
      lonnam     = "lon",
      latnam     = "lat",
      timenam    = "valid_time",
      do_chunks  = TRUE,
      outdir     = settings$dir_out_tidy_ssr,
      fileprefix = "ERA5Land_halfdeg.tot_ssr",
      overwrite  = settings$overwrite,
      # filter_lon_between_degrees = c(-1, 1), # TODO: only for development
      ncores     = settings$ncores_max,
      ...
    )
  } else {
    data.frame(input_path = settings$dir_in_ssr, msg = "No Surface net solar radiation directory found.")
  }

  ## Surface net thermal radiation ----------------------------------------------------
  res_str <- if (!is.na(settings$dir_in_str) &&
                 dir.exists(settings$dir_in_str)) {
    str_files <- list.files(
      settings$dir_in_str,
      recursive = TRUE,
      pattern   = "ERA5Land_UTCDaily\\.tot_str\\.[0-9]{4}_halfdeg\\.nc$",
      full.names = TRUE
    )
    stopifnot(length(str_files) > 0)
    map2tidy::map2tidy(
      nclist     = str_files,
      varnam     = "tot_str",
      lonnam     = "lon",
      latnam     = "lat",
      timenam    = "valid_time",
      do_chunks  = TRUE,
      outdir     = settings$dir_out_tidy_str,
      fileprefix = "ERA5Land_halfdeg.tot_str",
      overwrite  = settings$overwrite,
      # filter_lon_between_degrees = c(-1, 1), # TODO: only for development
      ncores     = settings$ncores_max,
      ...
    )
  } else {
    data.frame(input_path = settings$dir_in_str, msg = "No Surface net thermal radiation directory found.")
  }

  ## Climate -----------------------------------------------------------------
  res_climate_df <-
    if (!is.na(settings$dir_in_climate) && file.exists(settings$dir_in_climate)) {

      # HARDCODED CODE FOR DIFFERENT CLIMATE INPUT FILES:
      # Create 'res_climate':
      if (settings$source_climate == "watch-wfdei"){

        # data-product specific variable names
        vars <- c("Tair", "Rainf", "Snowf", "Qair", "SWdown", "PSurf", "Wind")

        settings$grid_climate <- list(
          lonnam = "lon",
          latnam = "lat",
          timenam = "timestp",
          timedimnam = "tstep"
        )

        # watch-wfdei files do not appear to have a CF-compliant time coordinate description.
        # Therefore we need to define a workaround with `fgetdate`
        fgetdate_function <- function(fn){
          first_day <- gsub(".*_WFDEI_((CRU)*_*)([0-9]*).nc","\\3", x = basename(fn)) |>
            lubridate::ym()
          return(seq(from = first_day,
                     to   = first_day + months(1) - 1, # go to the end of the month
                     by   = "day") |>
                   as.character())
        }

        # make files tidy for each variable
        res_climate_list <- purrr::map(
          vars,
          function(var) map2tidy::map2tidy(
            nclist  = list.files(
              file.path(settings$dir_in_climate, gsub("\\[VAR\\]", var, "[VAR]_daily")),
              pattern = "*.nc", # ".nc",  # XXX try
              full.names = TRUE
              ),
            varnam  = var,
            lonnam  = settings$grid_climate$lonnam,
            latnam  = settings$grid_climate$latnam,
            timenam = settings$grid_climate$timenam,
            do_chunks  = TRUE,
            outdir     = settings$dir_out_tidy_climate,
            fileprefix = paste0(var, "_daily_WFDEI"),
            overwrite  = settings$overwrite,
            fgetdate   = ifelse(is.function(fgetdate_function), fgetdate_function, NA),
            # filter_lon_between_degrees = c(-1, 1), # TODO: only for development
            ncores     = settings$ncores_max,  # parallel::detectCores()
            ...
            )
        )

        res_climate <- dplyr::bind_rows(res_climate_list)

      } else if (settings$source_climate == "ERA5Land.tp_ssrd_d2m_t2m_sp_u10_v10") {

        # fore ERA5Land.tp_ssrd_d2m_t2m_sp_u10_v10: make single tidy file containing all variables
        list_climate_files <- list.files(
          settings$dir_in_climate,
          recursive = TRUE,
          pattern = "ERA5Land.tp_ssrd_d2m_t2m_sp_u10_v10.[0-9]{4}.[0-9]{2}.nc",
          full.names = TRUE
          )

        stopifnot(length(list_climate_files) > 0)

        res_climate <- map2tidy::map2tidy(
          nclist     = list_climate_files,
          varnam     = c("tp","ssrd","d2m","t2m","sp","u10","v10"),
          lonnam     = "longitude",
          latnam     = "latitude",
          timenam    = "valid_time",
          do_chunks  = TRUE,
          outdir     = settings$dir_out_tidy_climate,
          fileprefix = "ERA5Land_hourly.tp_ssrd_d2m_t2m_sp_u10_v10",
          overwrite  = settings$overwrite,
          fgetdate   = NA,
          # filter_lon_between_degrees = c(1.0, 1.1)#, # TODO: only for development
          ncores     = settings$ncores_max,  # parallel::detectCores()
          ...
          )

        # # check:
        # readRDS(file.path(
        # "/data_2/scratch/era5land_munoz-sabater_2021/data",
        # "out_tidy/ERA5Land_hourly.tp_ssrd_d2m_t2m_sp_u10_v10_LON_+001.100.rds")) |>
        #   dplyr::slice(1) |> tidyr::unnest(data)

      } else if(settings$source_climate == "some-other-climate-source-to-be-defined") {
        # NOTE: add future sources here
        # define: vars, outfile_suffix, source_subdirectory, source_pattern, fgetdate_function
      } else {
        stop("
          Climate input need case-by-case modification of the code in grsofun.
          Your input to 'settings$source_climate' does not (yet) appear to be supported.")
      }

      # return(res_climate)
    } else {
      data.frame(input_path = settings$dir_in_climate, msg = "No climate file found.")
    }

  ## fAPAR ---------------------------------------------------------------------
  res_fapar <-
    if (!is.na(settings$file_in_fapar) && file.exists(settings$file_in_fapar)) {
      # HARDCODED CODE FOR DIFFERENT FAPAR INPUT FILES:
      if (settings$source_fapar == "modis"){
        map2tidy(
          nclist = settings$file_in_fapar,
          varnam = "fpar",
          lonnam = "lon",
          latnam = "lat",
          timenam = "time",
          do_chunks = TRUE,
          outdir = settings$dir_out_tidy_fapar,
          fileprefix = "MODIS-C061_MOD15A2H_LAI_FPAR_zmaw",
          overwrite = settings$overwrite,
          # filter_lon_between_degrees = c(-1,1), # TODO: only for development
          ncores     = settings$ncores_max,  # parallel::detectCores()
          ...
        )
      } else if(settings$source_fapar == "some-other-fapar-source-to-be-defined") {
        # NOTE: add future sources here
      } else {
        stop("
        FAPAR inputs need case-by-case modification of the code in grsofun.
        Your input to 'settings$source_fapar' does not (yet) appear to be supported.")
      }
    } else {
      data.frame(input_path = settings$file_in_fapar, msg = "No fapar file found.")
    }

  return(list(
    res_ssr        = res_ssr,
    res_str        = res_str,
    res_landmask   = res_landmask,
    res_whc        = res_whc,
    res_elv        = res_elv,
    res_canopy     = res_canopy,
    res_climate_df = res_climate_df,
    res_fapar      = res_fapar
  ))
}
