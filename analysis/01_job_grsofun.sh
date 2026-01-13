#!/bin/bash
#SBATCH --mail-user=ananda.kurth@unibe.ch
#SBATCH --job-name=test_0_grsofun
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
PREFIX=test_0
START=2000
END=2018

export R_LIBS_USER=/storage/homefs/ak24h624/R/x86_64-pc-linux-gnu-library/4.4

# Run the script
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
