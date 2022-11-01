#!/usr/bin/env bash

# Run with old parameters
./identify_fronts.R --nsmooth=2 --thresh_t=-5e-11 --thresh_g=0 --diagnostics -q data/q850/erai_q850_200001.nc -t data/t850/erai_t850_200001.nc -u data/u850/erai_u850_200001.nc -v data/v850/erai_v850_200001.nc output/contour_then_mask.nc

# Run with new parameters
./identify_fronts.R --diagnostics -q data/q850/erai_q850_200001.nc -t data/t850/erai_t850_200001.nc -u data/u850/erai_u850_200001.nc -v data/v850/erai_v850_200001.nc output/final.nc

