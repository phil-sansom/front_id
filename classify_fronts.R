#!/usr/bin/env -S Rscript --vanilla

# Load libraries
library(optparse)

# Load source
source("src/fronts_io.R")
source("src/classify_fronts.R")
source("src/history.R")

# Optional arguments
option_list <- list(
  make_option(c("--threshold","-t"), action = "store", type = "double",
              dest = "thresh_s", default = 1.5,
              help = "Speed threshold for front classification (m/s) [default: 1.5]")
)

# Argument parser
parser <- OptionParser(usage = "Usage: %prog [OPTION]... INFILE",
                       option_list = option_list,
                       description = "Classify meteorological fronts.")

# Parse arguments
args    <- commandArgs(TRUE)
history <- make_history("./classify_fronts.R", args)
argv    <- parse_args(parser, args = args, positional_arguments = 1)
opts    <- argv$options
args    <- argv$args
infile  <- args[1]

# Open input file
nci <- open_fronts(infile)

# Read attributes
attributes          <- read_attributes_fronts(nci)
attributes$thresh_s <- opts$thresh_s

# Read dimensions
dimensions <- read_dimensions_fronts(nci)
time       <- dimensions$time
time_units <- dimensions$time_units
calendar   <- dimensions$calendar

# Create output files
cold_file <- paste0(tools::file_path_sans_ext(infile),"_cold.nc")
warm_file <- paste0(tools::file_path_sans_ext(infile),"_warm.nc")
stat_file <- paste0(tools::file_path_sans_ext(infile),"_stat.nc")
ncc <- create_fronts(cold_file, time_units, calendar, attributes,
                     title   = "Meteorological cold fronts", 
                     history = history,
                     frspeed = TRUE)
ncw <- create_fronts(warm_file, time_units, calendar, attributes,
                     title   = "Meteorological warm fronts", 
                     history = history,
                     frspeed = TRUE)
ncs <- create_fronts(stat_file, time_units, calendar, attributes,
                     title   = "Meteorological stationary fronts", 
                     history = history,
                     frspeed = TRUE)

# Classify fronts and write out
for (i in 1:length(time)) {
  
  # Read fronts
  fronts <- read_fronts(nci, i)
  
  # Classify fronts
  buffer <- classify_fronts(fronts, opts$thresh_s)
  
  # Write cold fronts & increment counters
  ncc <- write_fronts(ncc, time[i], buffer$cold_fronts)
  
  # Write warm fronts & increment counters
  ncw <- write_fronts(ncw, time[i], buffer$warm_fronts)
  
  # Write stationary fronts & increment counters
  ncs <- write_fronts(ncs, time[i], buffer$stat_fronts)
  
} # i

# Close files
close_fronts(nci)
close_fronts(ncc)
close_fronts(ncw)
close_fronts(ncs)
