#!/bin/bash
#SBATCH --mail-user=ananda.kurth@unibe.ch
#SBATCH --job-name=PM_grsofun
#SBATCH --output=log/%x_%j.log
#SBATCH --error=log/%x_%j.err
#SBATCH --time=02:00:00
#SBATCH --mail-type=end,fail
#SBATCH --partition=icpu-stocker
#SBATCH --account=invest
#SBATCH --qos=job_icpu-stocker
#SBATCH --cpus-per-task=16

set -euo pipefail

module purge
module load R/4.4.2-gfbf-2024a UDUNITS/2.2.28-GCCcore-13.3.0 \
PROJ/9.4.1-GCCcore-13.3.0 GDAL/3.10.0-foss-2024a CDO/2.4.4-gompi-2024a

## To change
PREFIX=PM
START=2000
END=2018

export R_LIBS_USER=/storage/homefs/ak24h624/R/x86_64-pc-linux-gnu-library/4.4

# --- new: branch + shared lib config
export RSOFUN_REPO="geco-bern/rsofun"
export RSOFUN_REF="phydro"              # branch to install
export BRANCH_LIB="${R_LIBS_USER}/rsofun_phydro_lib"

mkdir -p "${BRANCH_LIB}"
# Prepend the branch lib to R_LIBS_USER so R (and worker processes) will search it first
export R_LIBS_USER="${BRANCH_LIB}:${R_LIBS_USER}"

# Optionally log which libs we are using
echo "R_LIBS_USER=${R_LIBS_USER}"
echo "Installing rsofun ${RSOFUN_REF} to ${BRANCH_LIB} (if missing) ..."

Rscript -e '
if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes", repos = "https://cloud.r-project.org")

branch_lib <- Sys.getenv("BRANCH_LIB")
if (!dir.exists(branch_lib))
  dir.create(branch_lib, recursive = TRUE, showWarnings = FALSE)

repo <- Sys.getenv("RSOFUN_REPO")
ref  <- Sys.getenv("RSOFUN_REF")

message("Installing ", repo, "@", ref, " into ", branch_lib, " ...")

remotes::install_github(
  paste0(repo, "@", ref),
  lib          = branch_lib,
  dependencies = TRUE,
  upgrade      = "always",
  force        = TRUE
)'


# Run the script (will see rsofun from BRANCH_LIB because R_LIBS_USER was prepended)
Rscript run_grsofun.R

# --- merge yearly NetCDFs to one monthly time series ---
OUTDIR=/storage/research/giub_geco/data_2/scratch/akurth/grsofun_output
NC_DIR="${OUTDIR}/${PREFIX}"
MERGED="${NC_DIR}/${PREFIX}_monthly_${START}_${END}.nc"

cd "${NC_DIR}"

files=()
for y in $(seq ${START} ${END}); do
  f="${PREFIX}_mon_${y}.nc"
  [ -f "${f}" ] || { echo "Missing: ${NC_DIR}/${f}" >&2; exit 1; }
  files+=("${f}")
done

cdo -O mergetime "${files[@]}" "${MERGED}"
cdo -s showname "${MERGED}"
cdo -s ntime "${MERGED}"
echo "✔ Merge complete: ${MERGED}"
