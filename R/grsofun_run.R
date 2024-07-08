#' @export
grsofun_run <- function(par, settings){

  if (settings$nthreads == 1){

    if (settings$ncores_max == 1){
      # Do not parallelize
      # out <- dplyr::tibble(ilon = 292) |>
      out <- dplyr::tibble(ilon = seq(settings$grid$len_ilon)) |>
          dplyr::mutate(out = purrr::map(
          ilon,
          ~grsofun_run_byilon(
            .,
            par,
            settings
          ))
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
      cl <- multidplyr::new_cluster(ncores) |>
        multidplyr::cluster_library(c("map2tidy",
                                      "dplyr",
                                      "purrr",
                                      "tidyr",
                                      "magrittr",
                                      "readr",
                                      "grsofun",
        )) |>
        multidplyr::cluster_assign(
          grsofun_run_byilon = grsofun_run_byilon,   # make the function known for each core
          read_forcing_byvar_byilon = read_forcing_byvar_byilon
        )

      # distribute computation across the cores, calculating for all longitudinal
      # indices of this chunk
      out <- dplyr::tibble(ilon = seq(settings$grid$len_ilon)) |>
        multidplyr::partition(cl) |>
        dplyr::mutate(out = purrr::map(
          ilon,
          ~grsofun_run_byilon(
            .,
            par,
            settings
          ))
        )

    }

  } else {
    # Create chunks of longitude indeces and send to separate threads
    # distribute to separate notes (distributed to cores within node is done
    # inside functions)
    purrr::map(
      seq(settings$nthreads),
      ~grsofun_run_bychunk(., settings$nthreads, par, settings)
    )

  }

}

#' @export
grsofun_run_bychunk <- function(chunk, nthreads, par, settings){

  # XXX: make this a system call for running a script containing this code

  # determine longitude indices to process
  vec_index <- map2tidy::get_index_by_chunk(
    chunk,                           # counter for chunks
    nthreads,                        # total number of chunks
    settings$grid_climate$len_ilon   # total number of longitude indices
  )

  # number of cores to use for this thread
  # keep one core free
  ncores <- ifelse(
    is.na(settings$ncores_max),
    max(parallel::detectCores() - 1, 1),
    min(settings$ncores_max, max(parallel::detectCores() - 1, 1))
  )

  # parallelize job
  # set up the cluster, sending required objects to each core
  cl <- multidplyr::new_cluster(ncores) |>
    multidplyr::cluster_library(c("map2tidy",
                                  "dplyr",
                                  "purrr",
                                  "tidyr",
                                  "magrittr",
                                  "readr",
                                  "grsofun",
                                  )) |>
    multidplyr::cluster_assign(
      grsofun_run_byilon = grsofun_run_byilon,   # make the function known for each core
      read_forcing_byvar_byilon = read_forcing_byvar_byilon
    )

  # distribute computation across the cores, calculating for all longitudinal
  # indices of this chunk
  out <- dplyr::tibble(ilon = vec_index) |>
    multidplyr::partition(cl) |>
    dplyr::mutate(out = purrr::map(
      ilon,
      ~grsofun_run_byilon(
        .,
        par,
        settings
      ))
    )

}

#' @export
grsofun_run_byilon <- function(ilon, par, settings){

  # xxx test: ilon = 388
  # for DE-Tha (DE-Tha  lon = 13.6, lat = 51.0, elv = 380 m), use ilon = 388 (lon = 13.75, lat = 50.75, sitename = grid_ilon_388_ilat_174)

  if (settings$save_drivers){
    filnam_drivers <- paste0(settings$dir_drivers, "/", settings$fileprefix, "_ilon_", ilon, ".rds")
  }

  if (settings$overwrite || !file.exists(filnam_drivers)){

    # get land mask - variable name hard coded
    df <- readr::read_rds(paste0(settings$dir_landmask_tidy, "WFDEI-elevation_ilon_", ilon, ".rds")) |>
      dplyr::rename(elv = elevation)

    # # get elevation
    # dplyr::left_join(
    #   readr::read_rds(paste0(settings$dir_elv_tidy, "ETOPO1_Bed_g_geotiff_halfdeg_ilon_", ilon, ".rds")) |>
    #     dplyr::rename(elv = ETOPO1_Bed_g_geotiff),
    #   dplyr::join_by(lon, lat)
    # ) |>

    # get rooting zone water storage capacity information. If missing, assume 200 mm.
    df_whc <- readr::read_rds(paste0(settings$dir_whc_tidy, "cwdx80_forcing_halfdeg_ilon_", ilon, ".rds"))
    if (nrow(df_whc) > 0){
      df <- df |>
        dplyr::left_join(
          df_whc |>
            dplyr::rename(whc = cwdx80_forcing),
          dplyr::join_by(lon, lat)
        )
    } else {
      df <- df |>
        dplyr::mutate(whc = 200)
    }
    df <- df |>
      dplyr::mutate(whc = ifelse(is.na(whc), 200, whc))

    # Read tidy climate forcing data by longitudinal band and convert units -
    # product-specific
    if (settings$source_climate == "watch-wfdei"){

      vars <- c("Tair", "Rainf", "Snowf", "Qair", "SWdown", "PSurf")

      # read tidy data for all variables and join into single data frame
      kfFEC <- 2.04
      df_climate <- purrr::map(
        vars,
        ~read_forcing_byvar_byilon(., ilon, settings)
      ) |>
        purrr::reduce(
          left_join,
          join_by(lon, lat, time)
        ) |>

        # convert units and rename
        dplyr::rowwise() |>
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
          )
        ) |>

        # XXX try
        mutate(
          netrad = NA,
          ccov = 0.5,
          co2 = 400,
        ) |>
        dplyr::select(
          lon,
          lat,
          date = time,
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
          tmax = Tair
        ) |>

        dplyr::group_by(lon, lat) |>
        tidyr::nest()

      # # xxx test
      # df_climate |>
      #   filter(lat == 50.75) |>
      #   unnest(data) |>
      #   ggplot(aes(date, vpd)) +
      #   geom_line()
      #
      # df_climate |>
      #   filter(lat == 50.75) |>
      #   unnest(data) |>
      #   visdat::vis_miss()

    }

    if (settings$source_fapar == "modis"){

      # read monthly fAPAR data
      df_fapar_mon <- readr::read_rds(paste0(settings$dir_fapar_tidy, "MODIS-C006_MOD15A2_LAI_FPAR_zmaw_ilon_", ilon, ".rds"))

      # check if something was read
      if ("data.frame" %in% class(df_fapar_mon)){
        avl_fapar <- TRUE
      } else {
        avl_fapar <- FALSE
      }

      if (avl_fapar){

        df_fapar_mon <- df_fapar_mon |>
          mutate(data = purrr::map(data, ~dplyr::rename(., date = time)))

        # # xxx problem: no decembers are read after 2014
        # df_fapar_mon |>
        #   unnest(data) |>
        #   select(date) |>
        #   distinct() |>
        #   arrange(date) |>
        #   View()

        # # xxx try
        # filter(lat == 50.75)

        # df_fapar_mon$data[[1]] |>
        #   ggplot(aes(date, fpar)) +
        #   geom_line()

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
          mutate(data = purrr::map(data, ~interpolate2daily_fpar(., ddf))) |>
          mutate(data = purrr::map(data, ~dplyr::select(., -fpar))) |>
          mutate(data = purrr::map(data, ~dplyr::rename(., fapar = fpar_daily)))

        # df_fapar$data[[1]] |>
        #   ggplot(aes(date, fpar)) +
        #   geom_line()

        # combine to capture all gridcells of the climate forcing
        # if fapar forcing is missing, set fapar = 0
        df_forcing <- df_climate |>
          tidyr::unnest(data) |>
          dplyr::left_join(
            df_fapar |>
              tidyr::unnest(data),
            by = c("lon", "lat", "date")
          ) |>
          dplyr::mutate(fapar = ifelse(is.na(fapar), 0, fapar)) |>
          dplyr::group_by(lon, lat) |>
          tidyr::nest()

      } else {
        # fapar not available - set to zero
        df_forcing <- df_climate |>
          mutate(data = purrr::map(data, ~dplyr::mutate(., fapar = 0)))
      }
    }

    # # xxx test
    # df_forcing |>
    #   filter(lat == 50.75) |>
    #   unnest(data) |>
    #   ggplot(aes(date, vpd)) +
    #   geom_line()
    #
    # df_forcing |>
    #   filter(lat == 50.75) |>
    #   unnest(data) |>
    #   visdat::vis_miss()

    # populate driver object
    if (settings$model == "pmodel"){

      df <- df |>

        # merge with forcing time series
        left_join(
          df_forcing |>
            dplyr::rename(forcing = data),
          join_by(lon, lat)
        ) |>

        mutate(ilat = seq(n())) |>

        # create simulation parameters (common for all)
        # construct site name from longitude and latitude indices
        dplyr::mutate(
          sitename = paste0("grid_ilon_", ilon, "_ilat_", ilat),
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
        tidyr::nest(
          params_siml = c(
            spinup,
            spinupyears,
            recycle,
            outdt,
            ltre,
            ltne,
            ltrd,
            ltnd,
            lgr3,
            lgn3,
            lgr4
          )
        ) |>

        # group site meta info
        tidyr::nest(
          site_info = c(
            lon,
            lat,
            elv,
            whc
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

    if (settings$save_drivers){
      message(paste("Writing file", filnam_drivers, "..."))
      readr::write_rds(df, file = filnam_drivers)
    }

  } else {

    df <- readr::read_rds(filnam_drivers)

  }

  # # some gridcells have no forcing because no fapar data is available
  # # fill them with NA forcing as forcing which triggers an empty rsofun run
  # df <- df |>
  #   mutate(isnull = purrr::map_lgl(forcing, ~is.null(.)))
  # df_forcing_empty <- df |>
  #   filter(!isnull) |>
  #   slice(1) |>
  #   pull(forcing)
  # df_forcing_empty <- df_forcing_empty[[1]] |>
  #   slice(1) |>
  #   mutate(
  #     across(
  #       c(temp, rain, vpd, ppfd, netrad, ccov, snow, co2, patm, tmin, tmax, fapar),
  #       ~ NA
  #       ))
  # df <- df |>
  #   mutate(forcing = purrr::map(forcing, ~ifelse(isnull, df_forcing_empty, forcing)))

  # # xxx test forcing (near DE-Tha)
  # df_fdk <- readRDS("~/data/FluxDataKit/v3.1/rsofun_driver_data_v3.1.rds")
  #
  # tmp <- df_fdk |>
  #   filter(sitename == "DE-Tha") |>
  #   select(forcing) |>
  #   unnest(forcing) |>
  #   rename_with(~stringr::str_c("fdk_", .), 2:17) |>
  #   right_join(
  #     df |>
  #       select(sitename, forcing) |>
  #       filter(sitename == "grid_ilon_388_ilat_174") |>
  #       unnest(forcing),
  #     by = c("date")
  #   )
  #
  # tmp |>
  #   visdat::vis_miss()
  #
  # skimr::skim(tmp)
  #
  # tmp |>
  #   ggplot() +
  #   geom_line(aes(date, fapar), color = "royalblue") +
  #   geom_line(aes(date, fdk_fapar))

  # run model
  out <- rsofun::runread_pmodel_f(
    drivers = df,
    par = par
  )

  # # xxx test run model
  # out2 <- rsofun::runread_pmodel_f(
  #   drivers = df_fdk |>
  #     filter(sitename == "DE-Tha") |>
  #     mutate(forcing = purrr::map(forcing, ~mutate(., patm = zoo::na.approx(patm)))),
  #   par = par
  # )
  #
  # # xxx test
  # tmp2 <- out2 |>
  #   select(data) |>
  #   unnest(data) |>
  #   rename_with(~stringr::str_c("fdk_", .), 2:21) |>
  #   right_join(
  #     out |>
  #       select(data) |>
  #       unnest(data),
  #     by = c("date")
  #   )
  #
  # tmp2 |>
  #   ggplot() +
  #   geom_line(aes(date, wcont), color = "royalblue") +
  #   geom_line(aes(date, fdk_wcont))

  # # xxx test
  # out |>
  #   filter(sitename == "grid_ilon_388_ilat_174") |>
  #   unnest(data) |>
  #   ggplot(aes(date, gpp)) +
  #   geom_line()

  outpath <- paste0(settings$dir_out, settings$fileprefix, "_ilon_", ilon, ".rds")
  message(paste("Writing file", outpath, "..."))
  readr::write_rds(out, file = outpath)

}

#' @export
read_forcing_byvar_byilon <- function(var, ilon, settings){

  if (settings$source_climate == "watch-wfdei"){
    df <- readr::read_rds(paste0(settings$dir_climate_tidy, var, "_daily_WFDEI_ilon_", ilon, ".rds")) |>
    tidyr::unnest(data)
  }

  return(df)
}
