#!/usr/bin/env -S Rscript --vanilla

# Load libraries
library(optparse)

# Load source
source("src/fronts_io.R")
source("src/maps_io.R")
source("src/find_sigma.R")
source("src/make_maps.R")
source("src/history.R")

# Optional arguments
option_list <- list(
  make_option(c("--xgrid","-x"), action = "store", type = "character",
              help = "Longitude grid description (xsize,xfirst,xinc)"),
  make_option(c("--ygrid","-y"), action = "store", type = "character",
              help = "Latitude grid description (ysize,yfirst,yinc)"),
  make_option("--sigma", action = "store", type = "double",
              help = "Manually specify standard deviation (degrees) for jittering when changing grids, setting --sigma=0 disables jittering")
)

# Argument parser
parser <- OptionParser(usage = "Usage: %prog [OPTION]... INFILE OUTFILE",
                       option_list = option_list,
                       description = "Regrid meteorological fronts.")

# Parse arguments
args    <- commandArgs(TRUE)
history <- make_history("./regrid_fronts.R", args)
argv    <- parse_args(parser, args = args, positional_arguments = 2)
opts    <- argv$options
args    <- argv$args
infile  <- as.character(args[1])
outfile <- as.character(args[2])

# Tolerance for coordinate checking
tol <- 0.001

# Open input file
nci <- open_fronts(infile)

# Read attributes from first file
attributes <- read_attributes_fronts(nci)

# Read dimensions
dimensions <- read_dimensions_fronts(nci)
time       <- dimensions$time
time_units <- dimensions$time_units
calendar   <- dimensions$calendar

# Is front speed data included?
frspeed <- exists("frspeed", nci$var)

# Original grid
lon0 <- seq(from       = attributes$xfirst, 
            by         = attributes$xinc,
            length.out = attributes$xsize)
lat0 <- seq(from       = attributes$yfirst,
            by         = attributes$yinc,
            length.out = attributes$ysize)
dlon   <- attributes$xinc
minlon <- attributes$xfirst
maxlon <- attributes$xfirst + attributes$xinc * (attributes$xsize - 1)

# Target longitude grid
if (exists("xgrid", opts)) {
  
  # Parse grid spec
  xgrid <- suppressWarnings(as.numeric(strsplit(opts$xgrid, ",")[[1]]))
  
  # Check grid string
  if (any(is.na(xgrid))) {
    stop ("Longitude grid format not recognised (xsize, xfirst, xinc)")
  }
  if (length(xgrid) != 3) {
    stop ("Longitude grid specification should have 3 components (xsize, xfirst, xinc)")
  }
  
  # Extract grid spec
  xsize  <- xgrid[1]
  xfirst <- xgrid[2]
  xinc   <- xgrid[3]
  xlast  <- xfirst + xinc * (xsize - 1)
  
  # Check grid spec
  if (xfirst < -360 - tol) {
    stop ("xfirst < -360")
  }
  if (+360 - tol < xfirst) {
    stop("xfirst >= +360")
  }
  if (xsize < 1) {
    stop("xsize < 1")
  }
  if (xinc <= 0) {
    stop("xinc <= 0")
  }
  if (+360 + tol < xlast) {
    stop("xfirst + xinc * (xsize - 1) > +360")
  }
  if (xsize*xinc > 360 + tol) {
    stop("xsize*xinc > 360")
  }
  
  # Transform bounds if needed
  if (abs((360 - dlon) - (maxlon - minlon)) < tol) {
    lon <- seq(xfirst, xlast, xinc)
    if (xfirst < -180 - tol) {
      xfirst <- lon[min(which(-180 - tol < lon))]
    } else if (-180 + xinc < xfirst + tol) {
      xfirst <- lon[min(which(+180 - tol < lon))] - 360
    }
    xlast <- xfirst + xinc * (xsize - 1)
  } else if (xfirst < -180 - tol && xlast < 0 + tol) {
    xfirst <- xfirst + 360
    xlast  <- xlast  + 360
  } else if (+180 - tol < xfirst) {
    xfirst <- xfirst - 360
    xlast  <- xlast  - 360
  }
  
  # Create grid
  lon <- seq(xfirst, xlast, xinc)
  
}  else {
  
  # Use original grid
  xinc <- attributes$xinc
  lon  <- lon0
  
} # xgrid

# Target latitude grid
if (exists("ygrid", opts)) {
  
  # Parse grid spec
  ygrid <- suppressWarnings(as.numeric(strsplit(opts$ygrid, ",")[[1]]))
  
  # Check grid string
  if (any(is.na(ygrid))) {
    stop("Latitude grid format not recognised (ysize, yfirst, yinc)")
  }
  if (length(ygrid) != 3) {
    stop("Latitude grid specification should have 3 components (ysize, yfirst, yinc)")
  }
  
  # Extract grid spec
  ysize  <- ygrid[1]
  yfirst <- ygrid[2]
  yinc   <- ygrid[3]
  ylast  <- yfirst + yinc * (ysize - 1)
  
  # Check grid spec
  if (yfirst < -90 - tol) {
    stop ("yfirst < -90")
  }
  if (+90 - tol <= yfirst) {
    stop("yfirst >= +90")
  }
  if (ysize < 1) {
    stop("ysize < 1")
  }
  if (yinc <= 0) {
    stop("yinc <= 0")
  }
  if (+90 + tol < ylast) {
    stop("yfirst + yinc * (ysize - 1) > +90")
  }
  if ((ysize - 1)*yinc > 180 + tol) {
    stop("(ysize - 1) * yinc > 180")
  }
  
  # Create grid
  lat <- seq(yfirst, ylast, yinc)
  
}  else {
  
  # Use original grid
  yinc <- attributes$yinc
  lat  <- lat0
  
} # ygrid

# Find sd for stochastic interpolation
if (exists("sigma", opts)) {
  sigma <- opts$sigma
  attributes$sigma <- opts$sigma
} else if (exists("xgrid", opts) || exists("ygrid", opts)) {
  sigma <- find_sigma(max(attributes$xinc, attributes$yinc), max(xinc, yinc))
  attributes$sigma <- sigma
} else {
  sigma <- 0
} # sigma

# Create output file
nco <- create_maps(outfile, lon, lat, time_units, calendar, attributes, 
                   title   = attributes$title, 
                   history = history, 
                   frspeed = frspeed)

# Loop over times
for (i in 1:length(time)) {
  
  # Load data
  fronts <- read_fronts(nci, i)
  
  # Create maps
  buffer <- make_maps(fronts, lon, lat, sigma, frspeed)
  
  # Write maps
  nco <- write_maps(nco, time[i], buffer)
  
} # i

# Close files
close_fronts(nci)
close_maps(nco)
