#' Reads daily output and aggregates temporally. Returning the data is optional.
#' By default, the aggregated data is written to tidy files by longitudinal bands.
#'
#' @export
#'
#' @param settings ...
#' @param return_data Flag whether to return loaded data in the R session. If FALSE
#'                    only RDS files are written to disk, if TRUE RDS files are
#'                    written to disk AND the data.frame is returned.
#'
#' @export
grsofun_collect <- function(
    settings,
    return_data = FALSE
){

  # Create vector of strings for identifying tidy files by longitudinal band
  df_lon_index <- map2tidy::get_df_lon_index(settings$grid)
  list_of_LON_str <- map2tidy::get_file_suffix(
    ilon = df_lon_index$lon_index,
    df_lon_index = df_lon_index
  )

  if (settings$ncores_max == 1){

    # un-parallel alternative
    df <- dplyr::tibble(LON_str = list_of_LON_str) |>
      dplyr::mutate(out = purrr::map(
        LON_str,
        ~grsofun_collect_byLON(
          .,
          settings,
          return_data = return_data
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
      multidplyr::cluster_library(c("dplyr",
                                    "purrr",
                                    "tidyr",
                                    "readr",
                                    "here"
      )) |>
      multidplyr::cluster_assign(
        grsofun_collect_byLON = grsofun_collect_byLON   # make the function known for each core
      )

    # distribute computation across the cores, calculating for all longitudinal
    # indices of this chunk
    df <- dplyr::tibble(LON_str = list_of_LON_str) |>
      multidplyr::partition(cl) |>
      dplyr::mutate(out = purrr::map(
        LON_str,
        ~grsofun_collect_byLON(
          .,
          settings,
          return_data = return_data
        ))
      ) |>
      dplyr::collect()

  }

  if (return_data){
    df <- df |>
      # filter out empty outputs
      dplyr::mutate(len = purrr::map_int(out, ~nrow(.))) |>
      dplyr::filter(len > 0) |>
      dplyr::select(-len) |>
      tidyr::unnest(out) |>
      dplyr::select(-LON_str)

    return(df)
  } else {
    return(NULL)
  }
}


#' @export
grsofun_collect_byLON <- function(
    LON_string, # e.g LON_string = "LON_+047.750"
    settings,
    return_data = FALSE
){

  outpath <- paste0(settings$dir_out, settings$fileprefix, LON_string, ".rds")
  message(paste("Reading file", outpath, "..."))
  ddf <- readr::read_rds(file = outpath)

  # filter out empty outputs
  ddf <- ddf |>
    dplyr::mutate(len = purrr::map_int(data, ~nrow(.))) |>
    dplyr::filter(len > 1) |>
    dplyr::select(-len)

  if (nrow(ddf) > 0){
    # aggregate temporally to mean
    # to monthly
    vars <- names(purrr::keep(settings$save, ~(.x == "mon")))
    mdf <- ddf |>
      tidyr::unnest(data) |>
      dplyr::mutate(
        year = lubridate::year(date),
        month = lubridate::month(date)
      ) |>
      dplyr::group_by(sitename, year, month) |>
      dplyr::summarise(dplyr::across(dplyr::all_of(vars), \(x) mean(x, na.rm = TRUE)), .groups = "drop") |>

      # add lon and lat to data frame
      left_join(
        ddf |>
          dplyr::select(sitename, site_info) |>
          tidyr::unnest(site_info) |>
          dplyr::select(sitename, lon, lat), # dplyr::select(sitename, lon, lat, fland)
        by = "sitename"
      )

  } else {
    mdf <- dplyr::tibble()
  }

  if (return_data){
    return(mdf)
  } else {
    # write to file
    outpath <- paste0(settings$dir_out, settings$fileprefix, "_mon", LON_string, ".rds")
    message(paste("Writing file", outpath, "..."))
    readr::write_rds(mdf, file = outpath)
  }

}
