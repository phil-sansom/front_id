#!/usr/bin/env -S Rscript --vanilla

# Load libraries
library(optparse)

# Load source
source("src/fronts_io.R")
source("src/history.R")

# Argument parser
parser <- OptionParser(usage = "Usage: %prog [OPTION]... INFILE... OUTFILE",
                       description = "Merge fronts from INFILE(s) and write to OUTFILE")

# Parse arguments
args    <- commandArgs(TRUE)
history <- make_history("./merge_fronts.R", args)
argv    <- parse_args(parser, args = args, positional_arguments = c(2, Inf))
opts    <- argv$options
args    <- argv$args
n_args  <- length(args)
infiles <- args[1:(n_args - 1)]
outfile <- args[n_args]

# Open connection to first file
nc <- open_fronts(infiles[1])

# Read attributes from first file
attributes <- read_attributes_fronts(nc)

# Read dimensions
dimensions <- read_dimensions_fronts(nc)
time       <- dimensions$time
time_units <- dimensions$time_units
calendar   <- dimensions$calendar

# Is front speed data included?
frspeed <- exists("frspeed", nc$var)

# Close connection
close_fronts(nc)

# Create output file
nco <- create_fronts(outfile, time_units, calendar, attributes,
                     title   = attributes$title, 
                     history = history,
                     frspeed = frspeed)

# Loop over infiles
for (infile in infiles) {
  
  # Open connection
  nci <- open_fronts(infile)
  
  # Read dimensions
  time <- read_dimensions_fronts(nci)$time
  
  # Loop over times
  for (i in 1:length(time)) { 
    
    # Read fronts
    fronts <- read_fronts(nci, i)
    
    # Write fronts
    nco <- write_fronts(nco, time[i], fronts)
    
  } # i
  
  # Close input file
  close_fronts(nci)
  
} # infile

# Close output file
close_fronts(nco)
