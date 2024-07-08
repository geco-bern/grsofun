#' @export
grsofun_tidy <- function(settings){

  # land mask and elevation in one
  error <- map2tidy::map2tidy(
    nclist = settings$file_landmask,
    varnam = "elevation",
    lonnam = "lon",
    latnam = "lat",
    do_chunks = TRUE,
    outdir = settings$dir_landmask_tidy,
    fileprefix = "WFDEI-elevation",
    overwrite = settings$overwrite
  )

  # root zone total whc
  error <- map2tidy::map2tidy(
    nclist = settings$file_whc,
    varnam = "cwdx80_forcing",
    lonnam = "lon",
    latnam = "lat",
    do_chunks = TRUE,
    outdir = settings$dir_whc_tidy,
    fileprefix = "cwdx80_forcing_halfdeg",
    overwrite = settings$overwrite
  )

  # # elevation
  # error <- map2tidy::map2tidy(
  #   nclist = settings$file_elv,
  #   varnam = "ETOPO1_Bed_g_geotiff",
  #   lonnam = "lon",
  #   latnam = "lat",
  #   do_chunks = TRUE,
  #   outdir = settings$dir_elv_tidy,
  #   fileprefix = "ETOPO1_Bed_g_geotiff_halfdeg"
  # )


  # climate
  if (settings$source_climate == "watch-wfdei"){

    # data-product specific variable names
    vars <- c("Tair", "Rainf", "Snowf", "Qair", "SWdown", "PSurf")

    # define grid of climate files
    settings$grid_climate <- list(
      lonnam = "lon",
      latnam = "lat",
      timenam = "timestp",
      timedimnam = "tstep"
    )

    # make files tidy for each variable
    error <- purrr::map(
      vars,
      ~grsofun_tidy_byvar(., settings)
    )

  }

  # fapar
  if (settings$source_fapar == "modis"){
    error <- map2tidy::map2tidy(
      nclist = settings$file_fapar,
      varnam = "fpar",
      lonnam = "lon",
      latnam = "lat",
      timenam = "time",
      timedimnam = "time",
      do_chunks = TRUE,
      outdir = settings$dir_fapar_tidy,
      fileprefix = "MODIS-C006_MOD15A2_LAI_FPAR_zmaw",
      single_basedate = TRUE,
      overwrite = TRUE
      # ncores = 2  # parallel::detectCores()
    )
  }

  return(settings)
}

#' @export
grsofun_tidy_byvar <- function(var, settings){

  # consider data product-specific directory structure and netcdf variable and
  # dimension names
  if (settings$source_climate == "watch-wfdei"){
    list_filnams <- list.files(
      paste0(settings$dir_climate, "/", var, "_daily/"),
      pattern = ".nc",
      full.names = TRUE
    )
  }

  # load and convert
  error <- map2tidy::map2tidy(
    nclist = list_filnams,
    varnam = var,
    lonnam = settings$grid_climate$lonnam,
    latnam = settings$grid_climate$latnam,
    timenam = settings$grid_climate$timenam,
    timedimnam = settings$grid_climate$timedimnam,
    do_chunks = TRUE,
    outdir = settings$dir_climate_tidy,
    fileprefix = paste0(var, "_daily_WFDEI"),
    single_basedate = FALSE,
    overwrite = settings$overwrite,
    # ncores = 2  # parallel::detectCores()
    ncores = 12  # parallel::detectCores()
  )

  return(error)
}
