#!/usr/bin/env Rscript

# script is called with two arguments for parallelization:
# 1. counter for chunks (e.g. # of each compute node)
# 2. total number of chunks (e.g. number of total compute nodes)

# Note that these arguments can be used to distribute over multiple nodes.
# Distribution over CPU cores of a single node is handled by multidplyr
# and argument ncores in the script.

# Example for 4 CPU-nodes:
# >./apply_cwd_global.R 1 4
# >./apply_cwd_global.R 2 4
# >./apply_cwd_global.R 3 4
# >./apply_cwd_global.R 4 4

# Example for 1 CPU-nodes:
# >./apply_cwd_global.R 1 1
# # When using this script directly from RStudio, not from the shell, specify
# args <- c(1, 1)

# to receive arguments to script from the shell
# args = commandArgs(trailingOnly=TRUE)

library(tidyverse)
library(map2tidy)
library(multidplyr)
library(here)

# source all R functions of this repo
source_files <- list.files(here::here("R/"), "*.R$")  # locate all .R files
purrr::map(paste0(here::here("R/"), source_files), ~source(.)) 

# specify all paths
source(here::here("analysis/paths_geco.R"))  # for GECO WS

# 2) Setup parallelization ------------------------------------------------
# 2a) Split job onto multiple nodes
#     i.e. only consider a subset of the files (others might be treated by
#     another compute node)
vec_index <- map2tidy::get_index_by_chunk(
  as.integer(args[1]),     # counter for compute node
  as.integer(args[2]),     # total number of compute node
  settings$grid$len_ilon   # total number of longitude indices
)

# 2b) Parallelize job across cores on a single node
ncores <- parallel::detectCores()   # number of cores of parallel threads

cl <- multidplyr::new_cluster(ncores) |>
  # set up the cluster, sending required objects to each core
  multidplyr::cluster_library(c("map2tidy",
                                "tidyverse",
                                "here",
                                "rsofun"
                              )) |>

  # make functions and other objects known for each core
  multidplyr::cluster_assign(
    settings     = settings,
    grsofun_tidy = grsofun_tidy
    )

# distribute computation across the cores, calculating for all longitudinal
# indices of this chunk

# 3) Process files --------------------------------------------------------
out <- tibble(in_fname = filnams[vec_index]) |>
  mutate(LON_string = gsub("^.*?(LON_[0-9.+-]*).rds$", "\\1", basename(in_fname))) |>
  select(-in_fname) |>
  multidplyr::partition(cl) |>    # comment this partitioning for development
  dplyr::mutate(out = purrr::map(
    LON_string,
    ~grsofun_tidy(
      .,
      settings = settings))
    ) |> 
  collect()



