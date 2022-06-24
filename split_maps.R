#!/usr/bin/env -S Rscript --vanilla

# Load libraries
library(optparse)

# Load source
source("src/history.R")
source("src/maps_io.R")

# Optional arguments
option_list <- list(make_option("--suffix", action = "store", type = "character", 
                                help = "Suffix to append to output file name")
)

# Argument parser
parser <- OptionParser(usage = "Usage: \n\t%prog [OPTION]... INFILE OBASE",
                       option_list = option_list,
                       description = "Split gridded front files by month and year.\n\nOperands:\n\tINFILE\n\t\tInput file containing gridded fronts\n\tOBASE\n\t\tBasename of files to write output to <OBASE>_<YYYYMM>_<SUFFIX>.nc")

args    <- commandArgs(TRUE)
history <- make_history("./split_maps.R", args)
argv    <- parse_args(parser, args = args, positional_arguments = 2)
opts    <- argv$options
args    <- argv$args
infile  <- args[1]
obase   <- args[2]

# Open connection to first file
nci <- open_maps(infile)

# Read attributes from first file
attributes <- read_attributes_maps(nci)

# Read dimensions
dimensions <- read_dimensions_maps(nci)
longitude  <- dimensions$longitude
latitude   <- dimensions$latitude
time       <- dimensions$time
time_units <- dimensions$time_units
calendar   <- dimensions$calendar

# Read times in ISO format
iso_time   <- system(paste("cdo","showtimestamp",infile),
                     intern        = TRUE, 
                     ignore.stderr = TRUE)
iso_time1  <- strsplit(iso_time[[1]], "[[:blank:]]+")[[1]]
iso_time2  <- strsplit(iso_time1[-1], "[-T:]")
years      <- sapply(iso_time2, function(x) x[1])
months     <- sapply(iso_time2, function(x) x[2])
yearmonths <- paste(years, months, sep = "-")
yearmonths <- unique(yearmonths)

# Do we need to write additional fields?
variables <- get_variables_maps(nci)
fields <- any(c("tfp", "maggrad"," frspeed") %in% variables)
frspeed <- "frspeed" %in% variables

# Loop over years
for (year in unique(years)) {
  
  # Loop over months
  for (month in unique(months)) {
    
    # Create output file
    if (exists("suffix", opts)) {
      outfile <- paste0(obase, "_", year, month, "_", opts$suffix, ".nc")
    } else {
      outfile <- paste0(obase, "_", year, month, ".nc")
    }
    nco <- create_maps(outfile, longitude, latitude, 
                       time_units, calendar, attributes, 
                       fields  = fields, 
                       history = history, 
                       frspeed = frspeed)
    
    # Extract times
    times <- which(years == year & months == month)
    
    # Loop over times
    for (i in times) {
      
      # Read maps
      maps <- read_maps(nci, i)
      
      # Write maps
      nco <- write_maps(nco, time[i], maps)
      
    } # i
    
    # Close output file
    close_maps(nco)
    
  } # month
  
} # year

# Close input file
close_maps(nci)
