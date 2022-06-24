#!/usr/bin/env bash

## Identify fronts
echo Identifying fronts...
qfile=data/q850/erai_q850_200001.nc
tfile=data/t850/erai_t850_200001.nc
ufile=data/u850/erai_u850_200001.nc
vfile=data/v850/erai_v850_200001.nc
outfile=output/erai_fronts_raw_all_200001.nc
./identify_fronts.R -q $qfile -t $tfile -u $ufile -v $vfile $outfile

## Classify fronts
echo Classifying fronts...
infile=output/erai_fronts_raw_all_200001.nc
./classify_fronts.R $infile
for ftype in cold warm stat; do
    source=output/erai_fronts_raw_all_200001_${ftype}.nc
    dest=output/erai_fronts_raw_${ftype}_200001.nc
    mv $source $dest
done

## Regrid fronts
echo Regridding fronts...
for ftype in all cold warm stat; do
    echo $ftype
    infile=output/erai_fronts_raw_${ftype}_200001.nc
    outfile=output/erai_fronts_gridded_${ftype}_200001.nc
    ./regrid_fronts.R $infile $outfile
done

## Combine fronts
echo Combining fronts...
cold=output/erai_fronts_gridded_cold_200001.nc
warm=output/erai_fronts_gridded_warm_200001.nc
stat=output/erai_fronts_gridded_stat_200001.nc
outfile=output/erai_fronts_gridded_combined_200001.nc
./combine_maps.R $cold $warm $stat $outfile

## Expand fronts
echo Expanding fronts...
for ftype in all cold warm stat; do
    echo $ftype
    infile=output/erai_fronts_gridded_${ftype}_200001.nc
    outfile=output/erai_fronts_expanded_${ftype}_200001.nc
    ./expand_fronts.R $infile $outfile
done
