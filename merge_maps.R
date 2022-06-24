#!/usr/bin/env -S Rscript --vanilla

# Load libraries
library(optparse)

# Load source
source("src/history.R")
source("src/maps_io.R")

# Argument parser
parser <- OptionParser(usage = "Usage: %prog [OPTION]... INFILE... OUTFILE",
                       description = "Merge maps from INFILE(s) and write to OUTFILE")

# Parse arguments
args    <- commandArgs(TRUE)
history <- make_history("./merge_maps.R", args)
argv    <- parse_args(parser, args = args, positional_arguments = c(2, Inf))
opts    <- argv$options
args    <- argv$args
n_args  <- length(args)
infiles <- args[1:(n_args - 1)]
outfile <- args[n_args]

# Open first file
nc <- open_maps(infiles[1])

# Read attributes from first file
attributes <- read_attributes_maps(nc)

# Read dimensions
dimensions <- read_dimensions_maps(nc)
time       <- dimensions$time

# Do we need to write additional fields?
variables <- get_variables_maps(nc)
fields    <- any(c("tfp", "maggrad", "frspeed") %in% variables)
frspeed   <- "frspeed" %in% variables

# Close file
close_maps(nc)

# Create output file
nco <- create_maps(outfile, dimensions$lon, dimensions$lat,
                   dimensions$time_units, dimensions$calendar, attributes, 
                   fields  = fields, 
                   title   = attributes$title, 
                   history = history, 
                   frspeed = frspeed)

# Write maps
for (infile in infiles) {
  
  # Open file
  nci <- open_maps(infile)
  
  # Read dimensions
  time <- read_dimensions_maps(nci)$time
  
  # Loop over times
  for (i in 1:length(time)) {
    
    # Read data
    buffer <- read_maps(nci, i)
    
    # Write data
    nco <- write_maps(nco, time[i], buffer)
    
  } # i
  
  # Close input file
  close_maps(nci)
  
} # infile

# Close output file
close_maps(nco)
