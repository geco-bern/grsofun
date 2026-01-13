#' @param par ...
#' @param settings ...
#'
#' @export
grsofun_run <- function(par, settings){

  # Create necessary output directories
  if (!is.na(settings$dir_out)){
    dir.create(settings$dir_out, recursive = TRUE, showWarnings = FALSE)
  }
  if (!is.na(settings$dir_out_drivers)){
    dir.create(settings$dir_out_drivers, recursive = TRUE, showWarnings = FALSE)
  }

  # Create vector of strings for identifying tidy files by longitudinal band
  df_lon_index <- map2tidy::get_df_lon_index(settings$grid)
  list_of_LON_str <- map2tidy::get_file_suffix(
    ilon = df_lon_index$lon_index,
    df_lon_index = df_lon_index
  )

  if (settings$nnodes == 1){
    if (settings$ncores_max == 1){
      # Do not parallelize
      # out <- dplyr::tibble(ilon = 292) |>
      out <- dplyr::tibble(ilon = seq_along(list_of_LON_str),
                           LON_str = list_of_LON_str)

      out$out <- purrr::map(
        out$LON_str,
        function(LON) {
          res <- grsofun_run_byLON(LON, par, settings)
          gc()
          return(res)
        }
      )


    } else {
      # Parallelise by longitudinal bands on multiple cores of a single node
      # number of cores to use for this thread
      # keep one core free
      ncores <- ifelse(
        is.na(settings$ncores_max),
        max(parallel::detectCores() - 1, 1),
        min(settings$ncores_max, max(parallel::detectCores() - 1, 1))
      )

      # parallelize job
      # set up the cluster, sending required objects to each core
      cluster_export_functions <- function(cl){
        multidplyr::cluster_assign(cl,
                                   grsofun_run_byLON        = grsofun_run_byLON,
                                   read_forcing_byvar_byLON = read_forcing_byvar_byLON,
                                   par                      = par,
                                   settings                 = settings
        )
      }

      cl <- multidplyr::new_cluster(ncores) |>
        multidplyr::cluster_library(c("map2tidy","dplyr","purrr","tidyr","readr","grsofun"))

      cluster_export_functions(cl)

      # distribute computation across the cores, calculating for all longitudinal
      # indices of this chunk
      out <- dplyr::tibble(ilon = seq_along(list_of_LON_str),
                           LON_str = list_of_LON_str) |>
        multidplyr::partition(cl) |>
        dplyr::mutate(out = purrr::map(
          LON_str,
          function(LON) {
            res <- grsofun_run_byLON(LON, par, settings)
            gc()
            res
          }
        )) |>
        dplyr::collect()

      gc()

    }

  } else {
    # Create chunks of longitude indices and send to separate threads
    # distribute to separate nodes (distributed to cores within node is done
    # inside functions)
    stop("Option nnodes > 1 is currently not implemented.")
    purrr::map(
      seq(settings$nnodes),
      ~grsofun_run_bychunk(., settings$nnodes, par, settings)
    )
  }
}

##' @export
# grsofun_run_bychunk <- function(chunk, nnodes, par, settings){
#
#   stop("Option nnodes > 1 is currently not implemented.")
#
#   # XXX: make this a system call for running a script containing this code
#
#   # determine longitude indices to process
#   list_of_LON_str <- map2tidy::get_index_by_chunk(
#     chunk,                           # counter for chunks
#     nnodes,                        # total number of chunks
#     len_ilon                         # total number of longitude indices
#   )
#
#   # number of cores to use for this thread
#   # keep one core free
#   ncores <- ifelse(
#     is.na(settings$ncores_max),
#     max(parallel::detectCores() - 1, 1),
#     min(settings$ncores_max, max(parallel::detectCores() - 1, 1))
#   )
#
#   # parallelize job
#   # set up the cluster, sending required objects to each core
#   cl <- multidplyr::new_cluster(ncores) |>
#     multidplyr::cluster_library(c("map2tidy",
#                                   "dplyr",
#                                   "purrr",
#                                   "tidyr",
#                                   "magrittr",
#                                   "readr",
#                                   "grsofun",
#                                   )) |>
#     multidplyr::cluster_assign(
#       grsofun_run_byLON = grsofun_run_byLON,   # make the function known for each core
#       read_forcing_byvar_byLON = read_forcing_byvar_byLON
#     )
#
#   # distribute computation across the cores, calculating for all longitudinal
#   # indices of this chunk
#   out <- dplyr::tibble(ilon = list_of_LON_str) |>
#     multidplyr::partition(cl) |>
#     dplyr::mutate(out = purrr::map(
#       ilon,
#       ~grsofun_run_byLON(
#         .,
#         par,
#         settings
#       ))
#     )
#
# }

# This function ...
# 1. reads data from tidy data file, given for one longitudinal band
# 2. contstructs the driver data frames
# 3. runs the model for each pixel in this longitudinal band
# 4. writes model output to file (for given longitudinal band)
#' @export
grsofun_run_byLON <- function(LON_string, par, settings){
  # e.g LON_string = "LON_+046.750"
  # for DE-Tha (DE-Tha  lon = 13.6, lat = 51.0, elv = 380 m),
  #            use (lon = 13.75, lat = 50.75, sitename = grid_LON_+013.750_LAT_+050.750)

  filnam_drivers <- file.path(settings$dir_out_drivers, paste0(settings$fileprefix, LON_string, ".rds"))
  filnam_output <- paste0(settings$dir_out, settings$fileprefix, LON_string, ".rds")

  if (settings$overwrite || !file.exists(filnam_output)){

    if (settings$overwrite || !file.exists(filnam_drivers)){

      # get land mask - variable name hard coded
      filnam <- paste0(settings$dir_out_tidy_landmask, "/WFDEI-elevation", LON_string, ".rds")

      if (!file.exists(filnam)){
        stop(paste("File does not exist:", filnam))
      }

      df <- readr::read_rds(filnam) |>
        dplyr::rename(elv = elevation)

      # # get elevation
      # dplyr::left_join(
      #   readr::read_rds(paste0(settings$dir_out_tidy_elv, "ETOPO1_Bed_g_geotiff_halfdeg_ilon_", ilon, ".rds")) |>
      #     dplyr::rename(elv = ETOPO1_Bed_g_geotiff),
      #   dplyr::join_by(lon, lat)
      # ) |>

      # get rooting zone water storage capacity information. If missing, assume 200 mm.
      filnam <- paste0(
        settings$dir_out_tidy_whc,
        "/",
        gsub(".nc", "" , basename(settings$file_in_whc)),
        LON_string,
        ".rds"
      )
      if (!file.exists(filnam)){
        stop(paste("File does not exist:", filnam))
      }
      df_whc <- readr::read_rds(filnam)
      if (nrow(df_whc) > 0){
        df <- df |>
          dplyr::left_join(
            df_whc |>
              (\(x) {
                nm <- if ("cwdx80_forcing" %in% names(x)) "cwdx80_forcing" else "whc_2m"
                dplyr::rename(
                  x, whc = !!rlang::sym(nm))
              })(),
            dplyr::join_by(lon, lat)
          )
      } else {
        df <- df |>
          dplyr::mutate(whc = 200)
      }
      rm(df_whc)

      df <- df |>
        dplyr::mutate(whc = ifelse(is.na(whc), 200, whc))

      # Canopy height
      filnam <- file.path(
        settings$dir_out_tidy_canopy,
        paste0("canopy_height", LON_string, ".rds")
      )
      if (!file.exists(filnam)){
        stop(paste("File does not exist:", filnam))
      }
      df_canopy_height <- readr::read_rds(filnam)
      if (nrow(df_canopy_height) > 0){
        df <- df |>
          dplyr::left_join(
            df_canopy_height |>
              dplyr::rename(canopy_height = Band1),
            dplyr::join_by(lon, lat)) |>
          dplyr::mutate(
            canopy_height = ifelse(
              is.na(canopy_height) | canopy_height < 0.05,
              -1,                      # sentinel for desert / barren
              canopy_height
            ),
            reference_height = canopy_height
          )
      } else {
        df <- df |>
          dplyr::mutate(
            canopy_height   = -1,
            reference_height = -1
          )
      }
      rm(df_canopy_height)

      # Read tidy climate forcing data by longitudinal band and convert units -
      # product-specific
      if (settings$source_climate == "watch-wfdei"){

        # SSR and STR
        fil_ssr <- file.path(settings$dir_out_tidy_ssr,
                             paste0("ERA5Land_halfdeg.tot_ssr", LON_string, ".rds"))
        fil_str <- file.path(settings$dir_out_tidy_str,
                             paste0("ERA5Land_halfdeg.tot_str", LON_string, ".rds"))

        if (!file.exists(fil_ssr)) stop("Missing SSR file: ", fil_ssr)
        if (!file.exists(fil_str)) stop("Missing STR file: ", fil_str)

        df_str <- readr::read_rds(fil_str) |>
          tidyr::unnest(data) |>
          dplyr::rename(str = tot_str)

        df_ssr <- readr::read_rds(fil_ssr) |>
          tidyr::unnest(data) |>
          dplyr::rename(ssr = tot_ssr)

        df_netrad <- dplyr::inner_join(df_ssr, df_str,
                                       by = c("lon", "lat", "datetime")) |>
          dplyr::mutate(
            datetime = as.Date(datetime),
            netrad = ((ssr/ 86400)  + (str/ 86400))   # J/m2 -> W/m2
          ) |>
          dplyr::select(lon, lat, datetime, netrad)
        rm(df_ssr, df_str); gc()


        # CO2 data
        df_co2 <- readr::read_csv(settings$file_in_co2, show_col_types = FALSE) |>
          dplyr::rename(co2 = mean)

        vars <- c("Tair", "Rainf", "Snowf", "Qair", "SWdown", "PSurf", "Wind")

        # read tidy data for all variables and join into single data frame
        kfFEC <- 2.04
        df_climate <- purrr::map(
          vars,
          ~read_forcing_byvar_byLON(., LON_string, settings)
        ) |>
          purrr::reduce(
            dplyr::left_join,
            dplyr::join_by(lon, lat, datetime)
          ) |>
          dplyr::rename(vwind = Wind) |>

          # convert units and rename
          dplyr::mutate(
            Tair = Tair - 273.15,  # K -> deg C
            ppfd = SWdown * kfFEC * 1.0e-6,  # W m-2 -> mol m-2 s-1
            vapr = rgeco::calc_vp(
              qair = Qair,
              patm = PSurf
            ),
            vpd = rgeco::calc_vpd(
              eact = vapr,
              tc = Tair,
              patm = PSurf
            ),
            date  = as.Date(datetime),
            year  = lubridate::year(datetime),
            ccov   = 0.5,
            co2    = 400
          ) |>
          dplyr::left_join(df_co2, by = "year") |>
          dplyr::mutate(co2 = ifelse(is.na(co2.y), co2.x, co2.y)) |>
          dplyr::select(-co2.x, -co2.y) |>
          dplyr::left_join(df_netrad, by = c("lon", "lat", "date" = "datetime")) |>
          dplyr::transmute(
            lon,
            lat,
            date,
            temp = Tair,
            rain = Rainf,
            vpd,
            ppfd,
            netrad,
            ccov,
            snow = Snowf,
            co2,
            patm = PSurf,
            tmin = Tair,
            tmax = Tair,
            vwind
          ) |>

          dplyr::group_by(lon, lat) |>
          tidyr::nest()

        df_climate <- df_climate |>
          # parse datetimes from string (output of map2tidy) to datetimes
          dplyr::mutate(data = purrr::map(data, ~dplyr::mutate(., date = lubridate::ymd(date))))

        rm(df_co2, df_netrad); gc()
      }

      if (settings$source_fapar == "modis"){

        # read monthly fAPAR data
        filnam <- paste0(settings$dir_out_tidy_fapar, "/MODIS-C061_MOD15A2H_LAI_FPAR_zmaw", LON_string, ".rds")
        if (!file.exists(filnam)){
          stop(paste("File does not exist: ", filnam))
        }

        df_fapar_mon <- readr::read_rds(filnam)

        # check if something was read
        if ("data.frame" %in% class(df_fapar_mon) && nrow(df_fapar_mon) > 0){
          avl_fapar <- TRUE
        } else {
          avl_fapar <- FALSE
        }

        if (avl_fapar){

          df_fapar_mon <- df_fapar_mon |>

            # map2tidy outputs 'datetime', but pmodel requires 'date'
            dplyr::mutate(data = purrr::map(data, ~dplyr::rename(., date = datetime))) |>

            # parse datetimes from string (output of map2tidy) to datetimes
            dplyr::mutate(data = purrr::map(data, ~dplyr::mutate(., date = lubridate::ymd(date))))

          # linearly interpolate monthly fAPAR values to daily
          dates <- df_fapar_mon$data[[1]]$date
          year_start <- min(lubridate::year(dates))
          year_end <- max(lubridate::year(dates))

          # create a data frame that spans all dates between start and end of simulation
          # consider only complete years
          ddf <- dplyr::tibble(
            date = seq(
              from = lubridate::ymd(paste0(year_start, "-01-01")),
              to = lubridate::ymd(paste0(year_end, "-12-31")),
              by = "days"
            ))

          # function to linearly interpolate (leaves trailing NAs)
          interpolate2daily_fpar <- function(df, ddf){
            ddf <- ddf |>
              dplyr::left_join(
                df,
                by = "date"
              ) |>
              dplyr::mutate(
                fpar_daily = zoo::na.approx(fpar, na.rm = FALSE)
              )

            # fill remaining with mean seasonal cycle
            meandf <- ddf |>
              dplyr::mutate(doy = lubridate::yday(date)) |>
              dplyr::group_by(doy) |>
              dplyr::summarise(fpar_meandoy = mean(fpar_daily, na.rm = TRUE))

            ddf <- ddf |>
              dplyr::mutate(doy = lubridate::yday(date)) |>
              dplyr::left_join(
                meandf,
                by = "doy"
              ) |>
              dplyr::mutate(fpar_daily = ifelse(is.na(fpar_daily), fpar_meandoy, fpar_daily)) |>
              dplyr::select(-fpar_meandoy, -doy)

            return(ddf)
          }

          df_fapar <- df_fapar_mon |>
            dplyr::mutate(data = purrr::map(data, ~interpolate2daily_fpar(., ddf))) |>
            dplyr::mutate(data = purrr::map(data, ~dplyr::select(., -fpar))) |>
            dplyr::mutate(data = purrr::map(data, ~dplyr::rename(., fapar = fpar_daily)))

          # combine to capture all gridcells of the climate forcing
          # if fapar forcing is missing, set fapar = 0
          df_forcing <- df_climate |>
            tidyr::unnest(data) |>
            dplyr::left_join(
              (df_fapar |>
                tidyr::unnest(data)),
              by = c("lon", "lat", "date")
            ) |>
            dplyr::mutate(fapar = ifelse(is.na(fapar), 0, fapar)) |>
            dplyr::group_by(lon, lat) |>
            tidyr::nest()
        } else {
          # fapar not available - set to zero
          df_forcing <- df_climate |>
            dplyr::mutate(data = purrr::map(data, ~dplyr::mutate(., fapar = 0)))
        }
      }

      # populate driver object
      if (settings$model == "pmodel"){

        df <- df |>

          # merge with forcing time series
          dplyr::left_join(
            df_forcing |>
              dplyr::rename(forcing = data),
            dplyr::join_by(lon, lat)
          ) |>

          # remove leap days from forcing early
          dplyr::mutate(
            forcing = purrr::map(
              forcing, ~ dplyr::filter(.,
                                       !(lubridate::month(date) == 2 & lubridate::day(date) == 29)
              )
            ),
            forcing_acclim = forcing
          ) |>

          # construct site name from longitude and latitude indices
          dplyr::mutate(
            # LAT_string = sprintf("LAT_%+08.3f", lat),
            # sitename = paste0("grid_", LON_string, "_", LAT_string)
            sitename = paste0("grid_", LON_string, "_", sprintf("LAT_%+08.3f", lat))
          ) |>

          # create simulation parameters (common for all)
          dplyr::mutate(
            spinup = TRUE,
            spinupyears = settings$spinupyears,
            recycle = settings$recycle,
            outdt = 1,
            ltre = FALSE,
            ltne = FALSE,
            ltrd = FALSE,
            ltnd = FALSE,
            lgr3 = TRUE,
            lgn3 = FALSE,
            lgr4 = FALSE,
          ) |>

          # group simulation parameters
          dplyr::mutate(
            use_gs     = settings$params_siml$use_gs,
            use_phydro = settings$params_siml$use_phydro,
            use_pml    = settings$params_siml$use_pml
          ) |>
          tidyr::nest(params_siml = c(
            spinup, spinupyears, recycle, outdt, ltre, ltne, ltrd, ltnd,
            lgr3, lgn3, lgr4,
            use_gs, use_phydro, use_pml
          )) |>

          # group site meta info
          tidyr::nest(
            site_info = c(
              lon,
              lat,
              elv,
              whc,
              canopy_height,
              reference_height
            )
          ) |>

          # put columns in order
          dplyr::select(
            sitename,
            params_siml,
            site_info,
            forcing
          )
      }

      if (!is.na(settings$dir_out_drivers)){
        message(paste("Writing file", filnam_drivers, "..."))
        readr::write_rds(df, file = filnam_drivers)
      }

    } else {

      df <- readr::read_rds(filnam_drivers)

    }


    # run model
    out <- rsofun::runread_pmodel_f(
      drivers = df,
      par = par
    )

    # ------------------------------------------------------------------
    # Complement with land fraction (not ice or water) from GICEW
    # ------------------------------------------------------------------
    filnam_gicew <- file.path(
      settings$dir_out_tidy_gicew,
      paste0("gicew", LON_string, ".rds")
    )

    if (!file.exists(filnam_gicew)){
      stop("File does not exist: ", filnam_gicew)
    }

    # GICEW is the land area fraction per grid cell (0–1)
    df_fland <- readr::read_rds(filnam_gicew) |>
      dplyr::select(lon, lat, fland = GICEW)

    # attach fland to the nested site_info for each site
    out <- out |>
      dplyr::mutate(
        site_info = purrr::map(
          site_info,
          ~ .x |>
            dplyr::left_join(df_fland, dplyr::join_by(lon, lat))
        )
      )

    message(paste("Writing file", filnam_output, "..."))
    readr::write_rds(out, file = filnam_output)
    rm(out)

  } else {

    message(paste("File already exists", filnam_output, "."))

  }

}

#' @export
read_forcing_byvar_byLON <- function(var, LON_string, settings){

  if (settings$source_climate == "watch-wfdei"){
    filnam <- paste0(settings$dir_out_tidy_climate, "/", var, "_daily_WFDEI", LON_string, ".rds")
    if (!file.exists(filnam)){
      stop(paste("File does not exist:", filnam))
    }
    df <- readr::read_rds(filnam) |>
      tidyr::unnest(data)
  }

  return(df)
}
