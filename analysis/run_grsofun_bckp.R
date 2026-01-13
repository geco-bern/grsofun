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
ncores <- max(as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", 1)) - 1, 1)
ncores <- min(ncores, 8)   # or 8, depending on memory

# -----------------------------------------------------------
# Model and I/O configuration
# -----------------------------------------------------------

settings <- list(
  ### simulation config:
  fileprefix = "PM-S0",
  ## Changed
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

  ### final model output
  dir_out = "/storage/research/giub_geco/data_2/scratch/akurth/grsofun_output/test_1/",
  dir_out_nc = "/storage/research/giub_geco/data_2/scratch/akurth/grsofun_output/test_1/",
  dir_out_drivers = "/storage/research/giub_geco/data_2/scratch/akurth/grsofun_input/test_1",
  save = list(aet = "mon"),
  overwrite = TRUE,

  ### tidy model input config:
  grid = list(
    lon_start = -179.75,
    dlon      = 0.5,
    len_ilon  = 720,
    lat_start = -89.75,
    dlat      = 0.5,
    len_ilat  = 360
  ),
  #################################
  # Simulation parameters
  params_siml = list(
    use_gs = FALSE,
    use_pml = FALSE,
    use_phydro = FALSE
  ),
  ##################################

  # Source
  source_fapar = "modis",
  source_climate = "watch-wfdei",

  # Dir in
  dir_in_ssr = "/storage/research/giub_geco/data_2/ERA5Land/remap",
  dir_in_str = "/storage/research/giub_geco/data_2/ERA5Land/remap",
  file_in_canopy = "/storage/research/giub_geco/data_2/vegheight_lang_2023/canopy_mean_0.5deg.nc",
  file_in_co2 = "/storage/research/giub_geco/data_2/global/co2_annmean_mlo.csv",
  dir_in_climate = "/storage/research/giub_geco/data_2/wfdei_weedon_2014/data",
  file_in_fapar = "/storage/research/giub_geco/data_2/modis_lai_fpar/MODIS-C061_MOD15A2H__LAI_FPAR__LPDAAC__GLOBAL_0.5degree__UHAM-ICDC__2000_2024__MON__fv0.03.nc",
  file_in_whc = "/storage/research/giub_geco/data_2/scratch/fbernhard/whc_stocker_2023/data/remap/cwdx80_forcing_0.5degbil.nc",
  file_in_landmask = "/storage/research/giub_geco/data_2/wfdei_weedon_2014/data/WFDEI-elevation.nc",
  file_in_elv = "/storage/research/giub_geco/data_2/wfdei_weedon_2014/data/WFDEI-elevation.nc",
  file_in_gicew = "/storage/research/giub_geco/data_2/gicew/gicew_halfdeg.cdf",

  # Dir out
  dir_out_tidy_ssr = "/storage/research/giub_geco/data_2/ERA5Land/remap/tidy",
  dir_out_tidy_str = "/storage/research/giub_geco/data_2/ERA5Land/remap/tidy",
  dir_out_tidy_canopy = "/storage/research/giub_geco/data_2/vegheight_lang_2023/tidy",
  dir_out_tidy_climate = "/storage/research/giub_geco/data_2/watch_wfdei/tidy",
  dir_out_tidy_fapar = "/storage/research/giub_geco/data_2/modis_lai_fpar/global/tidy",
  dir_out_tidy_whc = "/storage/research/giub_geco/data_2/mct_data/tidy",
  dir_out_tidy_landmask = "/storage/research/giub_geco/data_2/watch_wfdei/tidy",
  dir_out_tidy_elv = "/storage/research/giub_geco/data_2/watch_wfdei/tidy",
  dir_out_tidy_gicew = "/storage/research/giub_geco/data_2/gicew/tidy"
)

# -----------------------------------------------------------
# Model parameters
# -----------------------------------------------------------

par <- list(
  kphio              = 5.000000e-02, # chosen to be too high for demonstration
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
  #err_gpp            = 2.31415,
  #err_le             = 24.76610,
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
  #err_gpp            = 2.31061,
  #err_le             = 24.25638,
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
  # default: keep settings as declared, only override known runs
  if (settings$fileprefix == "PT") {
    # WHC: 2 m map
    settings$file_in_whc <- "/storage/research/giub_geco/data_2/whc_2m/remap/whc_2m_0.5.nc"
    settings$dir_out_tidy_whc = "/storage/research/giub_geco/data_2/whc_2m/remap/tidy"

    # simulation flags
    settings$params_siml <- list(use_gs = FALSE,
                                 use_pml = FALSE,
                                 use_phydro = FALSE)

    # output dirs
    settings$dir_out    <- "/storage/research/giub_geco/data_2/scratch/akurth/grsofun_output/PT/"
    settings$dir_out_drivers = "/storage/research/giub_geco/data_2/scratch/akurth/grsofun_input/PT"
    settings$dir_out_nc <- settings$dir_out
    dir_out    <- settings$dir_out
    dir_out_nc <- settings$dir_out_nc

    # parameters for this run
    par <- par_PT

  } else if (settings$fileprefix == "PM") {
    settings$file_in_whc <- "/storage/research/giub_geco/data_2/whc_2m/remap/whc_2m_0.5.nc"
    settings$dir_out_tidy_whc = "/storage/research/giub_geco/data_2/whc_2m/remap/tidy"

    settings$params_siml <- list(use_gs = TRUE,
                                 use_pml = TRUE,
                                 use_phydro = FALSE)

    settings$dir_out    <- "/storage/research/giub_geco/data_2/scratch/akurth/grsofun_output/PM/"
    settings$dir_out_drivers = "/storage/research/giub_geco/data_2/scratch/akurth/grsofun_input/PM"
    settings$dir_out_nc <- settings$dir_out
    dir_out    <- settings$dir_out
    dir_out_nc <- settings$dir_out_nc

    par <- par_PM

  } else if (settings$fileprefix == "PM-S0") {
    # Stocker WHC map
    settings$file_in_whc <- "/storage/research/giub_geco/data_2/scratch/fbernhard/whc_stocker_2023/data/remap/cwdx80_forcing_0.5degbil.nc"
    settings$dir_out_tidy_whc = "/storage/research/giub_geco/data_2/mct_data/tidy"
    settings$params_siml <- list(use_gs = TRUE,
                                 use_pml = TRUE,
                                 use_phydro = FALSE)

    settings$dir_out    <- "/storage/research/giub_geco/data_2/scratch/akurth/grsofun_output/PM-S0/"
    settings$dir_out_drivers = "/storage/research/giub_geco/data_2/scratch/akurth/grsofun_input/PM-S0"
    settings$dir_out_nc <- settings$dir_out
    dir_out    <- settings$dir_out
    dir_out_nc <- settings$dir_out_nc

    par <- par_PM_S0

  } else {
    message("settings$fileprefix not one of PT / PM / PM-S0 — leaving defaults in place.")
    # ensure plain dir_out exists for legacy code:
    dir_out    <- settings$dir_out
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
tictoc::tic("Run model")
error <- grsofun_run(par, settings)
tictoc::toc()
gc()

# -----------------------------------------------------------
# Collect model output data
# -----------------------------------------------------------

tictoc::tic("Collect model output")
df <- grsofun_collect(settings, return_data = TRUE)
tictoc::toc()
gc()

# -----------------------------------------------------------
# Save model output data
# -----------------------------------------------------------

timestamp <- format(Sys.time(), "%Y%m%d_%H%M")

# save as rds
saveRDS(df, file = file.path(
  settings$dir_out,
  paste0("final_aet_df_", settings$fileprefix, "_", timestamp, ".rds")
))

# save as csv
write.csv(df,
          file = file.path(
            settings$dir_out,
            paste0("final_aet_df_", settings$fileprefix, "_", timestamp, ".csv")
          ),
          row.names = FALSE)


# -----------------------------------------------------------
# Generate example AET map plots for Jan & Jul and save
# -----------------------------------------------------------

coast <- rnaturalearth::ne_coastline(scale = 110, returnclass = "sf")

# January
gg1 <- df |>
  filter(month == 1) |>
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = aet), show.legend = TRUE) +
  geom_sf(data = coast,
          colour = "black",
          linewidth = 0.3) +
  coord_sf(ylim = c(-60, 85), expand = FALSE) +
  scale_fill_viridis_c(name = expression(paste("gC m" ^ -2, "s" ^ -1)),
                       option = "cividis",
                       limits = c(0, 15)) +
  theme_void() +
  labs(subtitle = "January monthly mean")

# July
gg2 <- df |>
  filter(month == 7) |>
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = aet), show.legend = TRUE) +
  geom_sf(data = coast,
          colour = "black",
          linewidth = 0.3) +
  coord_sf(ylim = c(-60, 85), expand = FALSE) +
  scale_fill_viridis_c(name = expression(paste("gC m" ^ -2, "s" ^ -1)),
                       option = "cividis",
                       limits = c(0, 15)) +
  theme_void() +
  labs(subtitle = "July monthly mean")

cowplot::plot_grid(gg1, gg2, ncol = 1)
rm(gg1, gg2, coast)
gc()

# -----------------------------------------------------------
# Save to disk (tiny bookkeeping)
# -----------------------------------------------------------


# plot
year_tag <- if (!is.null(settings$year_start)) {
  as.character(settings$year_start)
} else {
  "unknown_year"
}

fig_dir <- dir_out
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
fig_file <- file.path(fig_dir,
                      paste0(
                        "aet_",
                        settings$fileprefix,
                        "_",
                        year_tag,
                        "_",
                        timestamp,
                        ".png"
                      ))

ggsave(fig_file, width = 6, height = 5)
message("Saved plot to: ", fig_file)
message("All done")
