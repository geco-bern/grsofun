settings <- list(
  fileprefix = "test",  # simulation name defined by the user
  model = "pmodel",     # in future could also be "biomee", but not yet implemented
  year_start = 2018,    # xxx not yet handled
  year_end = 2018,      # xxx not yet handled
  grid = list(          # a list specifying the grid which must be common to all forcing data
    lon_start = -179.75,
    dlon = 0.5,
    len_ilon = 720,
    lat_start = -89.75,
    dlat = 0.5,
    len_ilat = 360
    ),
  source_climate = "watch-wfdei",  # a string specifying climate forcing dataset-specific variables
  dir_climate = "/data/archive/wfdei_weedon_2014", # path to where climate forcing data is located
  dir_climate_tidy = "/data/archive/wfdei_weedon_2014/tidy/",  # path to where tidy climate forcing data is to be written
  source_fapar = "modis",   # a string specifying fAPAR forcing dataset-specific variables
  file_fapar = "/data/scratch/bstocker/data/MODIS-C006_MOD15A2_LAI_FPAR_zmaw/MODIS-C006_MOD15A2__LAI_FPAR__LPDAAC__GLOBAL_0.5degree__UHAM-ICDC__2000_2018__MON__fv0.02.nc",  # path to where fAPAR forcing data is located
  dir_fapar_tidy = "~/data/MODIS-C006_MOD15A2_LAI_FPAR_zmaw/tidy/",  # path to where tidy fAPAR forcing data is to be written
  file_whc = "/data/archive/???/cwdx80_forcing_halfdeg.nc",  # get from zenodo: https://doi.org/10.5281/zenodo.10885724 and add to archive
  dir_whc_tidy = "~/data/mct_data/tidy/",  # path to where tidy  root zone storage capacity forcing data is to be written
  file_landmask = "/data/archive/wfdei_weedon_2014/data/WFDEI-elevation.nc",    # path to where land mask data is located
  dir_landmask_tidy = "~/data/archive_legacy/landmasks/WFDEI-elevation_tidy/",    # path to where tidy land mask data is to be written
  file_elv = "/data/archive/wfdei_weedon_2014/data/WFDEI-elevation.nc",   # path to where elevation data is located
  dir_elv_tidy = "~/data/archive_legacy/landmasks/WFDEI-elevation_tidy/",   # path to where tidy elevation data is to be written
  save_drivers = TRUE,   # whether rsofun driver object is to be saved. Uses additional disk space but substantially speeds up grsofun_run().
  dir_drivers = here::here("input/tidy/"),  # path where rsofun drivers are to be written
  overwrite = FALSE,    # whether files with tidy forcing data and drivers are to be overwritten. If false, reads files if available instead of re-creating them.
  spinupyears = 10,     # model spin-up length
  recycle = 1,          # climate forcing recycling during the spinup
  dir_out = here::here("output/tidy/"),     # path for tidy model output
  dir_out_nc = "xxx",  # xxx not yet handled
  save = list(         # a named list where names correspond to variable names in rsofun output and the value is a string specifying the temporal resolution to which global output is to be aggregated. 
    gpp = "mon"
  ),
  nthreads = 1,   # distribute to multiple nodes for high performance computing - xxx not yet implemented
  ncores_max = 1  # number of parallel jobs, set to 1 for un-parallel run
)