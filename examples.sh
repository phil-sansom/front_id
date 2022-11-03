#!/usr/bin/env bash

# Run with old parameters
Rscript --vanilla identify_fronts.R --diagnostics --nsmooth=2 --thresh_t=-5e-11 --thresh_g=0 -q data/q850/erai_q850_200001.nc -t data/t850/erai_t850_200001.nc -u data/u850/erai_u850_200001.nc -v data/v850/erai_v850_200001.nc output/contour_then_mask.nc

# Run with new parameters
Rscript --vanilla identify_fronts.R --diagnostics -q data/q850/erai_q850_200001.nc -t data/t850/erai_t850_200001.nc -u data/u850/erai_u850_200001.nc -v data/v850/erai_v850_200001.nc output/final.nc

# Figure 1(b)
Rscript --vanilla examples/figure1b.R

# Figures 3(b) & 3(c)
Rscript --vanilla examples/figure3bc.R
