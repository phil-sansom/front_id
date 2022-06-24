#!/usr/bin/env -S Rscript --vanilla

# Load libraries
library(optparse)

# Load source
source("src/history.R")
source("src/maps_io.R")

# Argument parser
parser <- OptionParser(usage = "Usage: %prog COLD_FILE WARM_FILE STAT_FILE OUTFILE",
                       description = "Combine maps of meteorological fronts.\n\nOperands:\n\tCOLD_FILE\n\t\tFile containing cold fronts\n\tWARM_FILE\n\t\tFile containing warm fronts\n\tSTAT_FILE\n\t\tFile containing quasi-stationary fronts\n\tOUTFILE\n\t\tOutput file")

# Parse arguments
args      <- commandArgs(TRUE)
history   <- make_history("./combine_maps.R", args)
argv      <- parse_args(parser, args = args, positional_arguments = 4)
args      <- argv$args
cold_file <- as.character(args[1])
warm_file <- as.character(args[2])
stat_file <- as.character(args[3])
outfile   <- as.character(args[4])

# Open input files
ncc <- open_maps(cold_file)
ncw <- open_maps(warm_file)
ncs <- open_maps(stat_file)

# Read attributes from first file
attributes <- read_attributes_maps(ncc)

# Read dimensions from first file
dimensions <- read_dimensions_maps(ncc)
time       <- dimensions$time

# Do we need to write additional fields?
variables <- get_variables_maps(ncc)
fields    <- any(c("tfp", "maggrad", "frspeed") %in% variables)

# Create output file
nco <- create_maps(outfile, dimensions$lon, dimensions$lat,
                   dimensions$time_units, dimensions$calendar, attributes, 
                   fields     = fields, 
                   classified = TRUE, 
                   history    = history)

# Loop over times
for (i in 1:length(time)) {
  
  cold <- read_maps(ncc, i)
  warm <- read_maps(ncw, i)
  stat <- read_maps(ncs, i)
  
  # Combine data
  if (fields) {
    
    tfp     <- stat$tfp
    maggrad <- stat$maggrad
    frspeed <- stat$frspeed
    
    mask <- ! is.na(warm$tfp)
    tfp    [mask] <- warm$tfp    [mask]
    maggrad[mask] <- warm$maggrad[mask]
    frspeed[mask] <- warm$frspeed[mask]
    
    mask <- ! is.na(cold$tfp)
    tfp    [mask] <- cold$tfp    [mask]
    maggrad[mask] <- cold$maggrad[mask]
    frspeed[mask] <- cold$frspeed[mask]
  }
  
  # Write data
  buffer <- list(
    cold_fronts = cold$fronts, 
    warm_fronts = warm$fronts,
    stat_fronts = stat$fronts
  )
  if (fields) {
    buffer$tfp     = tfp
    buffer$maggrad = maggrad
    buffer$frspeed = frspeed
  }
  nco <- write_maps(nco, time[i], buffer)
  
} # i

# Close files
close_maps(ncc)
close_maps(ncw)
close_maps(ncs)
close_maps(nco)
