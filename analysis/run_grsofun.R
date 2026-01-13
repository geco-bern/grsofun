#' Global simulation using grsofun
#'
#' Demonstrates a full global rsofun simulation, using WATCH-WFDEI climate and
#' MODIS fAPAR forcing. This script prepares tidy input files, runs the P-model
#' over a global grid, and visualizes GPP output as monthly maps.
#'
#' @details The grsofun framework allows running {rsofun} over large gridded
#'   inputs by first tidying NetCDF inputs into gridcell-specific time series,
#'   running simulations in parallel, and collecting outputs back into a tidy
#'   structure.
#'
# -----------------------------------------------------------
# Global rsofun simulation with grsofun: WATCH-WFDEI + MODIS
# -----------------------------------------------------------
library(BayesianTools)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(cowplot)
library(map2tidy)
library(purrr)
library(parallel)
library(here)
library(sf)
library(tictoc)
library(terra)

message("Starting program..")

# Load all R scripts from the R/ directory
source_files <- list.files(here::here("R/"), pattern = "*.R$")
purrr::walk(paste0(here::here("R/"), source_files), source)
purrr::walk(here::here("analysis/grsofun_save_nc.R"), source)

ncores <- max(as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", 1)) - 1, 1)
ncores <- min(ncores, 8) # or 4, depending on memory

base_data_path <- "/storage/research/giub_geco/data_2"
fileprefix = "test_0"   # "PM", "PM-S0" or "PT"
# -----------------------------------------------------------
# Model and I/O configuration
# -----------------------------------------------------------

settings <- list(
  ### simulation config:
  fileprefix = fileprefix,
  model = "pmodel",
  year_start = 2000,
  # xxx not yet handled
  year_end = 2018,
  # xxx not yet handled
  spinupyears = 10,
  recycle = 1,

  ### HPC config:
  nnodes = 1,
  ncores_max = ncores,

  ### tidy model input config:
  grid = list(
    lon_start = -179.75,
    dlon      = 0.5,
    len_ilon  = 720,
    lat_start = -89.75,
    dlat      = 0.5,
    len_ilat  = 360
  ),

  # Simulation parameters
  params_siml = list(
    use_gs = FALSE,
    use_pml = FALSE,
    use_phydro = FALSE
  ),

  ### Model output
  save = list(aet = "mon", le = "mon", gpp = "mon"),
  overwrite = FALSE,

  # Source
  source_fapar = "modis",
  source_climate = "watch-wfdei",

  # Path settings
  dir_in_ssr = file.path(base_data_path, "ERA5Land", "remap"),
  dir_in_str = file.path(base_data_path, "ERA5Land", "remap"),
  file_in_canopy = file.path(
    base_data_path,
    "vegheight_lang_2023",
    "canopy_mean_0.5deg.nc"
  ),
  file_in_co2 = file.path(base_data_path, "global", "co2_annmean_mlo.csv"),
  dir_in_climate = file.path(base_data_path, "wfdei_weedon_2014", "data"),
  file_in_fapar = file.path(
    base_data_path,
    "modis_lai_fpar",
    "MODIS-C061_MOD15A2H__LAI_FPAR__LPDAAC__GLOBAL_0.5degree__UHAM-ICDC__2000_2024__MON__fv0.03.nc"
  ),
  file_in_whc = file.path(
    base_data_path,
    "scratch",
    "fbernhard",
    "whc_stocker_2023",
    "data",
    "remap",
    "cwdx80_forcing_0.5degbil.nc"
  ),
  file_in_landmask = file.path(
    base_data_path,
    "wfdei_weedon_2014",
    "data",
    "WFDEI-elevation.nc"
  ),
  file_in_elv = file.path(
    base_data_path,
    "wfdei_weedon_2014",
    "data",
    "WFDEI-elevation.nc"
  ),
  file_in_gicew = file.path(base_data_path, "gicew", "gicew_halfdeg.cdf"),

  # Dir out
  dir_out_tidy_ssr = file.path(base_data_path, "ERA5Land", "remap", "tidy"),
  dir_out_tidy_str = file.path(base_data_path, "ERA5Land", "remap", "tidy"),
  dir_out_tidy_canopy = file.path(base_data_path, "vegheight_lang_2023/tidy"),
  dir_out_tidy_climate = file.path(base_data_path, "watch_wfdei", "tidy"),
  dir_out_tidy_fapar = file.path(base_data_path, "modis_lai_fpar", "global", "tidy"),
  dir_out_tidy_whc = file.path(base_data_path, "mct_data", "tidy"),
  dir_out_tidy_landmask = file.path(base_data_path, "watch_wfdei", "tidy"),
  dir_out_tidy_elv = file.path(base_data_path, "watch_wfdei", "tidy"),
  dir_out_tidy_gicew = file.path(base_data_path, "gicew", "tidy"),

  ### final model output
  dir_out = file.path(
    "/storage/research/giub_geco/data_2/scratch/akurth/grsofun_output",
    paste0(fileprefix, "/")
  ),
  dir_out_nc = file.path(
    "/storage/research/giub_geco/data_2/scratch/akurth/grsofun_output",
    fileprefix
  ),
  dir_out_drivers = file.path(
    "/storage/research/giub_geco/data_2/scratch/akurth/grsofun_input",
    fileprefix
  )
)

# -----------------------------------------------------------
# Model parameters
# -----------------------------------------------------------

par <- list(
  kphio              = 5.000000e-02,
  # chosen to be too high for demonstration
  kphio_par_a        = -2.289344e-03,
  kphio_par_b        = 1.525094e+01,
  soilm_thetastar    = 1.577507e+02,
  soilm_betao        = 1.169702e-04,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,
  tau_acclim         = 20.0,
  kc_jmax            = 0.41
)

## Phydro parameters
par_PT <- list(
  kphio              = 0.04608,
  kphio_par_a        = -0.00100,
  kphio_par_b        = 23.71806,
  soilm_thetastar    = 5.42797,
  err_gpp            = 2.27776,
  err_le             = 44.72722,
  soilm_betao        = 1.169702e-04,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,
  tau_acclim         = 20.0,
  kc_jmax            = 0.41
)

par_PM <- list(
  kphio              = 0.04727,
  kphio_par_a        = -0.00100,
  kphio_par_b        = 24.02663,
  soilm_thetastar    = 145.72290,
  err_gpp            = 2.31415,
  err_le             = 24.76610,
  gw_calib           = 0.67554,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,
  tau_acclim         = 20.0,
  kc_jmax            = 0.41
)

par_PM_S0 <- list(
  kphio              = 0.04756,
  kphio_par_a        = -0.00099,
  kphio_par_b        = 24.06332,
  soilm_thetastar    = 398.99790,
  err_gpp            = 2.31061,
  err_le             = 24.25638,
  gw_calib           = 0.74075,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,
  tau_acclim         = 20.0,
  kc_jmax            = 0.41
)


# -----------------------------------------------------------
# Model settings for phydro runs
# -----------------------------------------------------------

if (exists("settings") && is.list(settings)) {
  if (settings$fileprefix == "PT") {
    # WHC: 2 m map
    settings$file_in_whc <- file.path(base_data_path, "whc_2m", "remap", "whc_2m_0.5.nc")
    settings$dir_out_tidy_whc <- file.path(base_data_path, "whc_2m", "remap", "tidy")

    # simulation flags
    settings$params_siml <- list(use_gs = FALSE,
                                 use_pml = FALSE,
                                 use_phydro = FALSE)

    # output dirs
    dir_out <- settings$dir_out
    dir_out_nc <- settings$dir_out_nc

    # parameters for this run
    par <- par_PT
  } else if (settings$fileprefix == "PM") {
    settings$file_in_whc <- file.path(base_data_path, "whc_2m", "remap", "whc_2m_0.5.nc")
    settings$dir_out_tidy_whc <- file.path(base_data_path, "whc_2m", "remap", "tidy")

    settings$params_siml <- list(use_gs = TRUE,
                                 use_pml = TRUE,
                                 use_phydro = FALSE)

    dir_out <- settings$dir_out
    dir_out_nc <- settings$dir_out_nc

    par <- par_PM
  } else if (settings$fileprefix == "PM-S0") {
    # Stocker WHC map
    settings$file_in_whc <- file.path(
      base_data_path,
      "fbernhard",
      "whc_stocker_2023",
      "data",
      "remap",
      "cwdx80_forcing_0.5degbil.nc"
    )
    settings$dir_out_tidy_whc <- file.path(base_data_path, "mct_data", "tidy")
    settings$params_siml <- list(use_gs = TRUE,
                                 use_pml = TRUE,
                                 use_phydro = FALSE)

    dir_out <- settings$dir_out
    dir_out_nc <- settings$dir_out_nc

    par <- par_PM_S0
  } else {
    message("settings$fileprefix not one of PT/PM/PM-S0 — leaving defaults in place.")
    # ensure plain dir_out exists for legacy code:
    dir_out <- settings$dir_out
    dir_out_nc <- settings$dir_out_nc
  }

  # ensure params_siml keys exist and are logical
  settings$params_siml <- modifyList(list(
    use_gs = FALSE,
    use_pml = FALSE,
    use_phydro = FALSE
  ),
  settings$params_siml)
  settings$params_siml <- lapply(settings$params_siml, as.logical)
}

print(settings)


# -----------------------------------------------------------
# Preprocess tidy input data from NetCDF
# -----------------------------------------------------------

# tictoc::tic("Tidying input")
# tidy_out <- grsofun_tidy(settings)
# tictoc::toc()
# gc()

# -----------------------------------------------------------
# Run grsofun model simulation
# -----------------------------------------------------------
# tictoc::tic("Run model")
# error <- grsofun_run(par, settings)
# tictoc::toc()
# gc()

# -----------------------------------------------------------
# Collect model output data
# -----------------------------------------------------------
tictoc::tic("Collect model output")
grsofun_collect(settings, return_data = FALSE)
tictoc::toc()
gc()

# -----------------------------------------------------------
# Collect model output data
# -----------------------------------------------------------
tictoc::tic("Save model output")
grsofun_save_nc(settings)
tictoc::toc()
gc()
