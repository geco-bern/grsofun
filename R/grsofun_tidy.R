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

  # check beforehand if all outdirs (or their ) are writable:
  # # TODO: deactivated because not yet working with subdirectories, i.e. whe
  # #       mkdir has to create mutliple subfolders...
  # is_writable <- function(path){file.access(path, mode = 2) == 0}
  # # assert_writable <- function(path){
  # #   is_writable(path) ||
  # #     stop(sprintf("Path %s is not writable. Please correct!", path))
  # # }
  # list_writable_directories <- lapply(
  #   settings[c(# outfiles of grsofun_tidy()
  #              "dir_out_tidy_landmask",
  #              "dir_out_tidy_whc",
  #              "dir_out_tidy_fapar",
  #              "dir_out_tidy_climate",
  #              # outfiles of grsofun_run()
  #              "dir_out_drivers",
  #              "dir_out"
  #              # outfiles of grsofun_collect()
  #              # "dir_out_nc"
  #              )],
  #   # assert_writable)
  #   is_writable)
  # if(suppressWarnings(!all(list_writable_directories))){stop(
  #   "Not all out directories are writable. Make sure you have the permissions for:",
  #   "\n",
  #   paste(capture.output(print(settings[
  #     names(list_writable_directories[!unlist(list_writable_directories)])
  #   ])), collapse = "\n")
  # )}



  # land mask and elevation in one
  res_landmask <- if (!is.na(settings$file_in_landmask) && file.exists(settings$file_in_landmask)) {
    map2tidy::map2tidy(
      nclist = settings$file_in_landmask,
      varnam = "elevation",
      lonnam = "lon",
      latnam = "lat",
      do_chunks = TRUE,
      outdir = settings$dir_out_tidy_landmask,
      fileprefix = "WFDEI-elevation",
      overwrite = settings$overwrite_intermediate,
      ncores     = settings$ncores_max,  # parallel::detectCores()
      ...
    )
  } else {
    data.frame(input_path = settings$file_in_landmask, msg = "No landmask file found.")
  }

  # root zone total whc
  res_whc <- if (!is.na(settings$file_in_whc) && file.exists(settings$file_in_whc)) {
    map2tidy::map2tidy(
      nclist = settings$file_in_whc,
      varnam = "cwdx80_forcing",
      lonnam = "lon",
      latnam = "lat",
      do_chunks = TRUE,
      outdir = settings$dir_out_tidy_whc,
      fileprefix = "cwdx80_forcing_halfdeg",
      overwrite = settings$overwrite_intermediate,
      ncores     = settings$ncores_max,  # parallel::detectCores()
      ...
    )
  } else {
    data.frame(input_path = settings$file_in_whc, msg = "No whc file found.")
  }

  # elevation
  res_elv <- if (!is.na(settings$file_in_elv) && file.exists(settings$file_in_elv)) {
    map2tidy::map2tidy(
      nclist = settings$file_in_elv,
      varnam = "elevation", # varnam = "ETOPO1_Bed_g_geotiff",
      lonnam = "lon",
      latnam = "lat",
      do_chunks = TRUE,
      outdir = settings$dir_out_tidy_elv,
      fileprefix = "WFDEI-elevation", # fileprefix = "ETOPO1_Bed_g_geotiff_halfdeg",
      overwrite = settings$overwrite_intermediate,
      ncores    = settings$ncores_max,  # parallel::detectCores()
      ...
    )
  } else {
    data.frame(input_path = settings$file_in_elv, msg = "No elv file found.")
  }

  # climate
  res_climate_df <-
    if (!is.na(settings$dir_in_climate) && file.exists(settings$dir_in_climate)) {

      # HARDCODED CODE FOR DIFFERENT CLIMATE INPUT FILES:
      if (settings$source_climate == "watch-wfdei"){

        # data-product specific variable names
        vars <- c("Tair", "Rainf", "Snowf", "Qair", "SWdown", "PSurf")
        source_subdirectory <- "[VAR]_daily"
        source_pattern      <- ".nc"
        outfile_suffix      <- "_daily_WFDEI"
        grid_climate_names <- list(
          lonnam = "lon",
          latnam = "lat",
          timenam = "tstep" # or timestp??
        )

        # watch-wfdei files do not appear to have a CF-compliant time coordinate description.
        # Therefore we need to define a workaround with `fgetdate`
        fgetdate_function <- function(fn){
          first_day <- gsub(".*_WFDEI_((CRU)*_*)([0-9]*).nc","\\3", x=basename(fn)) |>
            lubridate::ym()
          return(seq(from = first_day,
                     to   = first_day + months(1) - 1, # go to the end of the month
                     by   = "day") |>
                   as.character())
        }
        stopifnot(is.function(fgetdate_function) || is.na(fgetdate_function))

        # make files tidy for each variable
        res_climate_list <- purrr::map(
          vars,
          function(var) map2tidy::map2tidy(
            nclist  = list.files(
              file.path(settings$dir_in_climate, gsub("\\[VAR\\]",var,source_subdirectory)),
              pattern = source_pattern,
              full.names = TRUE),
            varnam  = var,
            lonnam  = grid_climate_names$lonnam,
            latnam  = grid_climate_names$latnam,
            timenam = grid_climate_names$timenam,
            do_chunks  = TRUE,
            outdir     = settings$dir_out_tidy_climate,
            fileprefix = paste0(var, outfile_suffix),
            overwrite  = settings$overwrite_intermediate,
            fgetdate   = ifelse(is.function(fgetdate_function), fgetdate_function, NA),
            # filter_lon_between_degrees = c(-1, 1), # TODO: only for development
            ncores     = settings$ncores_max,  # parallel::detectCores()
            ...
            )
        )

        dplyr::bind_rows(res_climate_list)

      } else if (settings$source_climate == "ERA5"){

        # data-product specific variable names
        var_mapping <- c(
          Tair = "t2m",
          Rainf = "tp",
          Vpd = "vpd_cf",
          SWdown = "ssrd",
          PSurf = "sp"
        )
        vars <- names(var_mapping)  # Use file names for directory operations
        source_subdirectory <- "[VAR]_daily"
        source_pattern      <- ".nc"
        outfile_suffix      <- "_daily_ERA5"
        grid_climate_names <- list(
          lonnam = "longitude",
          latnam = "latitude",
          timenam = "time" # or timestp??
        )
        fgetdate <- NA

        # make files tidy for each variable
        res_climate_list <- purrr::map(
          vars,
          function(var) {
            actual_var <- if(settings$source_climate == "ERA5") var_mapping[var] else var
            map2tidy::map2tidy(
              nclist  = list.files(
                file.path(settings$dir_in_climate, gsub("\\[VAR\\]",var,source_subdirectory)),
                pattern = source_pattern,
                full.names = TRUE),
              varnam  = actual_var,  # Use actual_var instead of var
              lonnam  = grid_climate_names$lonnam,
              latnam  = grid_climate_names$latnam,
              timenam = grid_climate_names$timenam,
              do_chunks  = TRUE,
              outdir     = settings$dir_out_tidy_climate,
              fileprefix = paste0(var, outfile_suffix),
              overwrite  = settings$overwrite_intermediate,
              fgetdate   =  NA,
              # filter_lon_between_degrees = c(-1, 1), # TODO: only for development
              ncores     = settings$ncores_max, # parallel::detectCores()
              ...
              )
          }
        )

        dplyr::bind_rows(res_climate_list)

      } else if(settings$source_climate == "some-other-climate-source-to-be-defined") {
        # NOTE: add future sources here
        # define: vars, outfile_suffix, source_subdirectory, source_pattern, fgetdate_function
      } else {
        stop("
          Climate input need case-by-case modification of the code in grsofun.
          Your input to 'settings$source_climate' does not (yet) appear to be supported.")
      }

    } else {
      data.frame(input_path = settings$file_in_whc, msg = "No climate file found.")
    }


  # fapar
  res_fapar <-
    if (!is.na(settings$file_in_fapar) && file.exists(settings$file_in_fapar)) {
      # HARDCODED CODE FOR DIFFERENT FAPAR INPUT FILES:
      if (settings$source_fapar == "modis"){
        map2tidy::map2tidy(
          nclist = settings$file_in_fapar,
          varnam = "fpar",
          lonnam = "lon",
          latnam = "lat",
          timenam = "time",
          do_chunks = TRUE,
          outdir = settings$dir_out_tidy_fapar,
          fileprefix = "MODIS-C006_MOD15A2_LAI_FPAR_zmaw",
          overwrite = settings$overwrite_intermediate,
          # filter_lon_between_degrees = c(-1,1), # TODO: only for development
          ncores     = settings$ncores_max#,  # parallel::detectCores()
          #...
        )
      } else if(settings$source_fapar == "some-other-fapar-source-to-be-defined") {
        # NOTE: add future sources here
      } else {
        stop("
        FAPAR inputs need case-by-case modification of the code in grsofun.
        Your input to 'settings$source_fapar' does not (yet) appear to be supported.")
      }
    } else {
      data.frame(input_path = settings$file_in_whc, msg = "No fapar file found.")
    }

  return(list(
    res_landmask   = res_landmask,
    res_whc        = res_whc,
    res_elv        = res_elv,
    res_climate_df = res_climate_df,
    res_fapar      = res_fapar
  ))
}
