# front_id

Meteorological front identification and classification using the method of Hewson (1998).

Run/see test.sh for examples of usage.

Run examples.sh to reproduce Figure 1(b) and Figures 3(b) and 3(c) from the accompanying paper.

References:

Sansom, P.G. and Catto, J.L. (2022), Improved objective identification of meteorological fronts: a case study with ERA-Interim. Geoscientific Model Development.


## Primary functions

### identify_fronts.R

Identifies fronts in wet-bulb potential temperature field field. Requires either wet-bulb potential temperature alone, or air temperature and either specific humidity or relative humidity as inputs. If the eastward wind and northward wind are also supplied, then front speed is calculated so that fronts can be classified in a later step.

Usage:

./identify_fronts.R --wfile=WFILE OUTFILE\
./identify_fronts.R --tfile=TFILE --qfile=QFILE OUTFILE\
./identify_fronts.R --tfile=TFILE --rfile=RFILE OUTFILE

where

- --wfile=WFILE: file containing wet-bulb potential temperature
- --tfile=TFILE: file containing air temperature
- --qfile=QFILE: file containing specific humidity
- --rfile=RFILE: file containing relative humidity
- OUTFILE: output file

with options:

- --nsmooth=NSMOOTH: number of smoothing passes to use [default: 8]
- --thresh_t=THRESH_T: thermal front parameter threshold (K/m^2) [default: -1.5e11]
- --thresh_g=THRESH_G: Magnitude of gradient threshold (K/m) [default: 7.3e-6]
- --minlength=MINLENGTH: Minimum length of front (km) [default: 251]
- --searchrad=SEARCHRAD: Search radius for line joining (km) [default: 170]
- --pressure=PRESSURE Pressure level of input variables (Pa) [default: 85000]
- --tolerance=TOLERANCE Tolerance for coordinate checking (degrees) [default: 0.001]
- --level=LEVEL: level number to use if input files contain multiple pressure levels
- --domain=DOMAIN:	Limit the domain for analysis (lon1,lon2,lat1,lat2)
# - --interpolate=INTERPOLATE: Interpolate input date to alternative grid spacing (degrees)
- --wname=WNAME: variable name for wet-bulb potential temperature
- --tname=TNAME: variable name for air temperature
- --qname=QNAME: variable name for specific humidity
- --rname=RNAME: variable name for relative humidity
- --ufile=UFILE: file containing eastward wind
- --vfile=VFILE: file containing northward wind
- --uname=UNAME: variable name for eastward wind
- --vname=VNAME: variable name for northward wind
- --fourthorder: use fourth order accurate finite differences (second order by default)
- --includepoles: grid boxes at +/- 90N are excluded by default but can be included if required
- --diagnostics: Output thetaw, front locating parameter, thermal front parameter, magnitude of gradient and (optionally) front speed fields
- --nofronts: Do not identify or output fronts, useful in combination with --diagnostics.
- --help: displays help

If the extent of the data allows, it is recommended to expand the domain by nsmooth + 2 (or nsmooth + 4 if --fourthorder) grid boxes to allow for edge effects of numerical differentiation and smoothing.

Output format:

NetCDF continuous ragged array, see: https://cfconventions.org/Data/cf-conventions/cf-conventions-1.9/cf-conventions.html#_contiguous_ragged_array_representation

Dimensions:

time:    time
nfronts: number of fronts at time t (dimension: time)
npoints: number of points in front  (dimension: fronts)

Output variables:

longitude: longitude of front points                 (dimension: points)
latitude:  latitude of front points                  (dimension: points)
tfp:       thermal front parameter at front location (dimension: points)
maggrad:   magnitude of gradied at front location    (dimension: points)
frspeed:   front speed for classification            (dimension: points)

Fronts are written sequentially, it's easiest to read all the fronts from one time step. To do this
- get the number of fronts at the first time step from nfronts(1)
- read the number of points in the first front by reading npoints(j=1) from longitude, latiutude, tfp, maggrad, frspeed
- read each consecutive front by reading the the next npoints(j+1) from longitude, latitude, tfp, maggrad, frspeed
- once you have read nfronts(1) fronts, increment your time counter and get the number of points nfronts(i+1) at the next time step, then keep reading the next npoints(j+1)

To only read the fronts from a particular time step, sum the number of fronts in all preceeding time steps, sum the number of points in all the fronts corresponding to the previous time steps, then start reading from there. See function read_fronts() in src/fronts_io.R for example of how to read all the fronts from a particular time step.



### classify_fronts.R

Classifies fronts based on speed of movement towards warm air side of front.

Usage:

./classify_fronts.R infile

where

- infile is a file produced by identify_fronts.R, filter_fronts.R or merge_fronts.R

with options

- --threshold=THRESHOLD: the speed threshold for front classification (m/s) [default: 1.5]
- --help: displays help

Cold, warm and stationary fronts are output to separate files in the same directory as infile.

Fronts with speed less than -threshold are classified as cold, fronts with speed greater than +threshold are classified as warms, all other fronts are classified as quasi-stationary.

Output format:

Same as identify_fronts.R


### regrid_fronts.R

Outputs maps of identified fronts, (stochastically) interpolated to a specified grid resolution.

Usage

./regrid_fronts.R infile outfile

where

- infile is a file produced by identify_fronts.R, classify_fronts.R, filter_fronts.R, merge_fronts.R or split_fronts.R
- outfile is the file for output

with options

- --xgrid=XGRID: Longitude grid description (xsize,xfirst,xinc)
- --ygrid=YGRID: Latitude grid description (ysize,yfirst,yinc)
- --sigma=SIGMA: Manually specify standard deviation (degrees) for jittering when changing grids
- --help: displays help

If no grid is specified is not specified, then the native grid of the data will be used and no interpolation is necessary. 
Otherwise stochastic interpolation is performed in order to avoid over-weighting some points on the new grid.
For consistency, whole fronts rather than individual points are shifted by a random amount. 
The randomisation is controlled by a standard deviation automatically determined by the function and stored in the attributes.
The randomisation can be controlled manual using --sigma option, setting --sigma=0 disables jittering.

Output format:

NetCDF with longitude x latitude x time dimensions for the following fields:

fronts:  ID of front. IDs are unique and consecutive. IDs are resused each time step.
tfp:     thermal front parameter at front location.
maggrad: magnitude of the gradient at the front location.
frspeed: front speed for classifying into cold, warm and quasi-stationary.


### expand_fronts.R

Expand fronts by a specified amount to form a footprint.

Usage:

./expand_fronts.R infile outfile

where

- infile is a file produced by regrid_fronts.R or merge_maps.R
- outfile is the file for output

with options

- --threshold=THRESHOLD: the radius to expand fronts by (km) [default: 251]
- --help: displays help

Output format:

NetCDF with longitude x latitude x time dimensions for the following fields:

fronts: 1/0 indicator for presence of front

Doesn't make sense to keep ID for expanded fronts since can overlap.


## Auxilliary scripts


### filter_fronts.R

Filters already identified fronts for more extreme values of thresh_t, thresh_g, minlength or searchrad.

Usage

./filter_fronts.R infile outfile

where

- --infile is a file produced by identify_fronts.R or merge_fronts.R
- --outfile: the file for output

with options

- --thresh_t: the new thermal front parameter threshold
- --thresh_g: the new magnitude of gradient threshold
- --minlength: the new minimum front length (km)
- --searchrad: the new search radius for line joining (km)
- --help: displays help

Output format:

Same as identify_fronts.R


### combine_maps.R

Combines cold, warm and stationary fronts into a single file.

Usage

./combine_maps.R cold_file warm_file stat_file outfile

where

- cold_file is file produced by regrid_fronts.R or merge_maps.R containing cold fronts
- warm_file is file produced by regrid_fronts.R or merge_maps.R containing warm fronts
- stat_file is file produced by regrid_fronts.R or merge_maps.R containing stationary fronts
- outfile is the file for output

Type:

./combine_maps.R --help

for help.

Output format:

NetCDF with longitude x latitude x time dimensions for the following fields:

cold_fronts:  ID of cold fronts. IDs are unique and consecutive. IDs are resused each time step.
warm_fronts:  ID of warm fronts. IDs are unique and consecutive. IDs are resused each time step.
stat_fronts:  ID of quasi-stationary fronts. IDs are unique and consecutive. IDs are resused each time step.
tfp:     thermal front parameter at front location.
maggrad: magnitude of the gradient at the front location.
frspeed: front speed for classifying into cold, warm and quasi-stationary.

IDs of cold, warm and stationary fronts are different from the unclassified fronts, i.e., a cold and a warm section of the same front might have different IDs.


### climatology.R

Produces a climatology of fronts.

Usage

./climatology.R infiles outfile

where

- infiles is any number of input files produced by regrid_fronts.R or merge_maps.R
- outfile is the file for output

Type:

./climatology.R --help

for help.


### merge_fronts.R

Merges fronts from different times into a single file.

Usage

./merge_fronts.R infiles outfile

where

- infiles is any number of input files produced by identify_fronts.R, classify_fronts.R or filter_fronts.R
- outfile is the file for output

Type:

./merge_fronts.R --help

for help.

Output format:

Same as identify_fronts.R


### merge_maps.R

Merges maps of fronts from different times into a single file.

Usage

./merge_maps.R infiles outfile

where

- infiles is any number of input files produced by regrid_fronts.R or expand_fronts.R
- outfile is the file for output

Type:

./merge_maps.R --help

for help.

Output format:

Same as regrid_fronts.R


### calibrate.R

Determine the number of smoothing cycles required to make the distribution of the thermal front parameter in a new model comparable to a reference model.

Usage

./calibrate.R [OPTION]... --wfile=WFILE\
./calibrate.R [OPTION]... --tfile=TFILE --qfile=QFILE\
./calibrate.R [OPTION]... --tfile=TFILE --rfile=RFILE

where 

-	--wfile=WFILE: the wet-bulb potential temperature file
-	--tfile=TFILE: the air temperature file
-	--qfile=QFILE: the specific humidity file
-	--rfile=RFILE: the relative humidity file

with options

-	--wfile-ref=WFILE-REF:
		Reference wet-bulb potential temperature file
-	--tfile-ref=TFILE-REF:
		Reference air temperature file [default: data/t850/erai_t850_200001.nc]
-	--qfile-ref=QFILE-REF:
		Reference specific humidity file [default: data/q850/erai_q850_200001.nc]
-	--rfile-ref=RFILE-REF:
		Reference relative humidity file
-	--nsmooth=NSMOOTH:
		Number of smoothing passes to use for reference analysis [default: 8]
-	--pressure=PRESSURE:
		Pressure level the input data are measured on (Pa) [default: 85000]
- --domain=DOMAIN:
    Limit the domain for analysis (lon1,lon2,lat1,lat2)
-	--level-ref=LEVEL-REF:
		Level number to use if reference input files contain multiple pressure levels
-	--wname-ref=WNAME-REF:
		Variable name in reference wet-bulb potential temperature file
-	--tname-ref=TNAME-REF:
		Variable name in reference air temperature file
-	--qname-ref=QNAME-REF:
		Variable name in reference specific humidity file
-	--rname-ref=RNAME-REF:
		Variable name in reference relative humidity file
-	--level=LEVEL:
		Level number to use if input files contain multiple pressure levels
-	--wname=WNAME:
		Variable name in wet-bulb potential temperature file
-	--tname=TNAME:
		Variable name in air temperature file
-	--qname=QNAME:
		Variable name in specific humidity file
-	--rname=RNAME:
		Variable name in relative humidity file
-	--quantile=QUANTILE:
		Quantile to match [default: 0.25]
-	--quantity=QUANTITY:
		Quantity to match (tfp/maggrad) [default: tfp]
-	--lower=LOWER:
		Lower end of the interval to be searched [default: 0]
-	--upper=UPPER:
		Upper end of the interval to be searched
-	--optol=OPTTOL:
		The desired accuracy for optimization [default: 1]
- --tol=TOL: 
    The tolerance for coordinate checking (degrees) [default: 0.001]


### split_fronts.R

Split front files by month and year, similar to 'cdo splityearmon'. This relies on 'cdo showtimestamp' for time handling since R does not have a good interface to UDUNITS.

Usage

	./split_fronts.R [OPTION]... INFILE OBASE

-	INFILE is an input file containing raw fronts to be split
-	OBASE is the basename of the files to write output to <OBASE>_<YYYYMM>_<SUFFIX>.nc

Options:
	--suffix=SUFFIX
		Suffix to append to output file name

	-h, --help
		Show this help message and exit


### split_maps.R

Split gridded or expanded front files by month and year, similar to 'cdo splityearmon'. This relies on 'cdo showtimestamp' for time handling since R does not have a good interface to UDUNITS.

Usage

	./split_maps.R [OPTION]... INFILE OBASE

-	INFILE is an input file containing gridded fronts to be split
-	OBASE is the basename of the files to write output to <OBASE>_<YYYYMM>_<SUFFIX>.nc

Options:
	--suffix=SUFFIX
		Suffix to append to output file name

	-h, --help
		Show this help message and exit


Output format:

Same as regrid_fronts.R
