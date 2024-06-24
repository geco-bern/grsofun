
# Reads daily output and aggregates temporally. Returning the data is optional.
# By default, the aggregated data is written to tidy files by longitudinal bands.
#' @export
grsofun_collect <- function(
    settings,
    return_data = FALSE
){

  if (settings$ncores_max == 1){

    # un-parallel alternative
    df <- tibble(ilon = seq(settings$grid$len_ilon)) |>
      dplyr::mutate(out = purrr::map(
        ilon,
        ~grsofun_collect_byilon(
          .,
          settings,
          return_data = return_data
        ))
      )

    if (return_data){
      df <- df |>
        dplyr::mutate(len = purrr::map_int(out, ~nrow(.))) |>
        dplyr::filter(len > 0) |>
        dplyr::select(-len) |>
        tidyr::unnest(out)

    }


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
                                    "here",
                                    "magrittr"
      )) |>
      multidplyr::cluster_assign(
        grsofun_collect_byilon = grsofun_collect_byilon   # make the function known for each core
      )

    # distribute computation across the cores, calculating for all longitudinal
    # indices of this chunk
    df <- tibble(ilon = seq(settings$grid$len_ilon)) |>
      multidplyr::partition(cl) |>
      dplyr::mutate(out = purrr::map(
        ilon,
        ~grsofun_collect_byilon(
          .,
          settings,
          return_data = return_data
        ))
      )

    if (return_data){
      df <- df |>
        collect() |>
        dplyr::mutate(len = purrr::map_int(out, ~nrow(.))) |>
        dplyr::filter(len > 0) |>
        dplyr::select(-len) |>
        tidyr::unnest(out)
    }

  }

  if (return_data){
    return(df)
  } else {
    return(NULL)
  }
}


#' @export
grsofun_collect_byilon <- function(
    ilon,
    settings,
    return_data = FALSE
){

  outpath <- paste0(settings$dir_out, settings$fileprefix, "_ilon_", ilon, ".rds")
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
    vars <- names(settings$save[settings$save == "mon"])
    mdf <- ddf |>
      tidyr::unnest(data) |>
      dplyr::mutate(
        year = lubridate::year(date),
        month = lubridate::month(date)
      ) |>
      dplyr::group_by(sitename, year, month) |>
      summarise(across(all_of(vars), \(x) mean(x, na.rm = TRUE)), .groups = "drop") |>

      # add lon and lat to data frame
      left_join(
        ddf |>
          dplyr::select(sitename, site_info) |>
          tidyr::unnest(site_info) |>
          dplyr::select(sitename, lon, lat),
        by = "sitename"
      )

  } else {
    mdf <- tibble()
  }

  if (return_data){
    return(mdf)
  } else {
    # write to file
    outpath <- paste0(settings$dir_out, settings$fileprefix, "_mon_ilon_", ilon, ".rds")
    message(paste("Writing file", outpath, "..."))
    readr::write_rds(file = outpath)
    return(NULL)
  }

}
