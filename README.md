# grsofun: Global rsofun runs

*Author: Benjamin Stocker*

## Approach

The {grsofun} package provides functions that wrap a call to `runread_pmodel_f()` from the {rsofun} package. This call is a point-scale simulation of the P-model. Functions of {grsofun} enable spatially distributed simulations on each gridcell and a parallelisation by gridcells for high computational speed.

The implementation follows a *tidy* data paradigm. All model input and output are organised as tidy data frames where forcing and output time series are nested inside cells and each row represents one gridcell. Because `runread_pmodel_f()` requires full forcing time series sequences and because reading full time series with global coverage is commonly limited by memory, the forcing data first has to be re-organised as tidy data frames and split into separate files for each longitudinal band. After running the simulations, outputs, written as tidy data frames into separate files for each longitudinal band, have to be collected again before further processing of spatial fields.

Each of these steps is implemented by a separate {grsofun} function. Functions share the `settings` argument which contains a list of all user-specified and simulation-specific information. Model parameters (which may be calibrated using {rsofun}) are provided as another separate argument.

The workflow of running a global simulation of one year with WATCH-WFDEI climate forcing and MODIS FPAR is demonstrated below.

## Workflow

Load libraries for this vignette.

``` r
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(cowplot)
```

### Specify settings

Note: currently, {grsofun} is set up run in demo mode with a prescribed constant CO2 and globally uniform constant cloud cover. This is to be accounted for by reading from data.

``` r
settings <- list(
  # simulation config:
  fileprefix  = "test",   # simulation name defined by the user
  model       = "pmodel", # in future could also be "biomee", but not yet implemented
  year_start  = 2018,     # xxx not yet handled
  year_end    = 2018,     # xxx not yet handled
  spinupyears = 10,       # model spin-up length
  recycle     = 1,        # climate forcing recycling during the spinup

  # HPC config:
  nnodes = 1,   # distribute to multiple nodes in a HPC environment: nnodes=1 no distribution, otherwise nnodes number of nodes to use in a HPC cluster setup to use (e.g. UBELIX) - xxx not yet implemented"
  ncores_max = 1, # number of multiple CPU-cores used per node for shared-memory parallel programming (e.g. with models like OpenMP)"

  # final model output
  dir_out = tempdir(),     # path for tidy model output
  dir_out_nc = tempdir(),  # xxx not yet handled
  save = list(            # a named list where names correspond to variable names
    gpp = "mon"           #   in rsofun output and the value is a string specifying
  ),                      #   the temporal resolution to which global output is to be aggregated.

  # intermediate model output
  dir_out_drivers = tempdir(),  # If a path is provided rsofun driver object is to be saved. Uses additional disk space but substantially speeds up grsofun_run(). If dir_out_drivers is 'NA' driver object are not stored.
  overwrite = FALSE,      # whether files with tidy forcing data and drivers are to be overwritten. If false, reads files if available instead of re-creating them.

  ### tidy model input config:
  grid = list( # a list specifying the grid which must be common to all forcing data
    lon_start = -179.75,
    dlon      = 0.5,
    len_ilon  = 720,
    lat_start = -89.75,
    dlat      = 0.5,
    len_ilat  = 360
  ),

  source_climate = "watch-wfdei",  # a string specifying climate forcing dataset-specific variables
  dir_in_climate = "/data/archive/wfdei_weedon_2014/data", # path to where climate forcing data is located
  dir_out_tidy_climate = tempdir(),  # path to where tidy climate forcing data is to be written

  source_fapar = "modis",  # a string specifying fAPAR forcing dataset-specific variables
  file_in_fapar = "/data/scratch/bstocker/MODIS-C006_MOD15A2_LAI_FPAR_zmaw/MODIS-C006_MOD15A2__LAI_FPAR__LPDAAC__GLOBAL_0.5degree__UHAM-ICDC__2000_2018__MON__fv0.02.nc",  # path to where fAPAR forcing data is located
  dir_out_tidy_fapar = tempdir(),  # path to where tidy fAPAR forcing data is to be written

  file_in_whc = "/data/archive/whc_stocker_2023/data/remap/cwdx80_forcing_0.5degbil.nc",
  dir_out_tidy_whc = tempdir(), # path to where tidy root zone storage capacity forcing data is to be written
  file_in_landmask = "/data/archive/wfdei_weedon_2014/data/WFDEI-elevation.nc",  # path to where land mask data is located
  dir_out_tidy_landmask = tempdir(), # path to where tidy land mask data is to be written

  file_in_elv = "/data/archive/wfdei_weedon_2014/data/WFDEI-elevation.nc", # path to where elevation data is located
  dir_out_tidy_elv = tempdir() # path to where tidy elevation data is to be written
)
```

### Specify model parameters

``` r
par <- list(
  kphio              = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
  kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
  kphio_par_b        = 1.0,
  soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
  soilm_betao        = 0.0,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
  tau_acclim         = 30.0,
  kc_jmax            = 0.41
)
```

### Make forcing tidy

Convert forcing data, provided as NetCDF, into a tidy format (time series data frames for each gridcell along rows) and saved for longitudinal bands in a binary format.

```{r eval=FALSE}
tidy_out <- grsofun_tidy(settings)
```

### Run model

Read forcing data and construct the driver object for an rsofun run. Run the model and save outputs in a tidy format in `.rds` files for each longitudinal band.

``` r
error <- grsofun_run(par, settings)
```

### Collect outputs

Read outputs saved in separate files for each longitudinal band. Collect data from all longitudinal bands and apply temporal aggregation steps as specified by the settings.

This can either write the aggregated data again to tidy files by longitudinal band, or return the temporally aggregated data into the R environment.

``` r
df <- grsofun_collect(settings, return_data = TRUE)
```

## Example plot

Plot a map.

``` r
coast <- rnaturalearth::ne_coastline(
  scale = 110,
  returnclass = "sf"
  )

# January
gg1 <- df |>
  filter(month == 1) |>
  ggplot() +
  geom_raster(
    aes(x = lon, y = lat, fill = gpp),
    show.legend = TRUE
    ) +
  geom_sf(
    data = coast,
    colour = 'black',
    linewidth = 0.3
  )  +
  coord_sf(
    ylim = c(-60, 85),
    expand = FALSE
  ) +
  scale_fill_viridis_c(
    name = expression(paste("gC m"^-2, "s"^-1)),
    option = "cividis",
    limits = c(0, 15)
    ) +
  theme_void() +
  labs(
    subtitle = "January monthly mean"
  )

# July
gg2 <- df |>
  filter(month == 7) |>
  ggplot() +
  geom_raster(
    aes(x = lon, y = lat, fill = gpp),
    show.legend = TRUE
    ) +
  geom_sf(
    data = coast,
    colour = 'black',
    linewidth = 0.3
  )  +
  coord_sf(
    ylim = c(-60, 85),
    expand = FALSE
  ) +
  scale_fill_viridis_c(
    name = expression(paste("gC m"^-2, "s"^-1)),
    option = "cividis",
    limits = c(0, 15)
    ) +
  theme_void() +
  labs(
    subtitle = "July monthly mean"
  )

cowplot::plot_grid(
  gg1,
  gg2,
  ncol = 1
)
```

![Global distribution of monthly mean GPP in January (top) and July (bottom).](man/figures/gpp_demo.png)
