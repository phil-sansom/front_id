#!/usr/bin/env -S Rscript --vanilla

# Load libraries
library(optparse)

# Load source
source("src/fronts_io.R")
source("src/history.R")
source("src/extract_fronts.R")

# Optional arguments
option_list <- list(
  make_option(c("--thresh_t"), action = "store", type = "double",
              help = "Threshold for thermal front parameter"),
  make_option(c("--thresh_g"), action = "store", type = "double",
              help = "Threshold for magnitude of gradient"),
  make_option(c("--minlength"), action = "store", type = "double",
              help = "Minimum front length (km)"),
  make_option(c("--searchrad"), action = "store", type = "double",
              help = "Search radius for front joining (km)")
)

# Argument parser
parser <- OptionParser(usage = "Usage: %prog [OPTION]... INFILE OUTFILE",
                       option_list = option_list,
                       description = "Filter meteorological fronts.")

# Parse arguments
args    <- commandArgs(TRUE)
history <- make_history("./filter_fronts.R", args)
argv    <- parse_args(parser, args = args, positional_arguments = 2)
opts    <- argv$options
args    <- argv$args
infile  <- args[1]
outfile <- args[2]

# Open input file
nci <- open_fronts(infile)

# Read attributes
attributes <- read_attributes_fronts(nci)
if (exists("thresh_t" , opts)) {
  attributes$thresh_t  <- opts$thresh_t
}
if (exists("thresh_g" , opts)) {
  attributes$thresh_g  <- opts$thresh_g
}
if (exists("minlength", opts)) {
  attributes$minlength <- opts$minlength
}
if (exists("searchrad", opts)) {
  attributes$searchrad <- opts$searchrad
}

# Read dimensions
dimensions <- read_dimensions_fronts(nci)
time       <- dimensions$time
time_units <- dimensions$time_units
calendar   <- dimensions$calendar

# Create output file
nco <- create_fronts(outfile, time_units, calendar, attributes,
                     title   = attributes$title, 
                     history = history, 
                     frspeed = exists("frspeed", nci$var))

# Loop over times
for (i in 1:length(time)) {
  
  # Read fronts
  fronts <- read_fronts(nci, i)
  
  # Filter fronts
  filtered <- extract_fronts(fronts, attributes)
  
  # Write fronts
  nco <- write_fronts(nco, time[i], filtered)
  
} # i

# Close files
close_fronts(nci)
close_fronts(nco)
