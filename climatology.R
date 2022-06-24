#!/usr/bin/env -S Rscript --vanilla

# Load libraries
library(optparse)
library(ncdf4)

# Load source
source("src/history.R")
source("src/maps_io.R")

# Argument parser
parser <- OptionParser(usage = "Usage: %prog [OPTION]... INFILE... OUTFILE",
                      description = "Compute climatology from INFILE(s) and write to OUTFILE")

# Parse arguments
args    <- commandArgs(TRUE)
history <- make_history("./climatology.R", args)
argv    <- parse_args(parser, args = args, positional_arguments = c(2, Inf))
opts    <- argv$options
args    <- argv$args
n_args  <- length(args)
infiles <- args[1:(n_args - 1)]
outfile <- args[n_args]

# Open input file
nc <- open_maps(infiles[1])

# Read attributes
attributes <- read_attributes_maps(nc)

# Read dimensions
dimensions <- read_dimensions_maps(nc)
lon        <- dimensions$longitude
lat        <- dimensions$latitude
time_units <- dimensions$time_units
calendar   <- dimensions$calendar
units      <- strsplit(time_units, " ")[[1]][1]

# Close file
close_maps(nc)

# Initialize storage
climatology <- matrix(0, length(lon), length(lat))
time        <- numeric(0)

# Loop over files
for (infile in infiles) {
  
  # Open input file
  nci <- open_maps(infile)
  
  # Extract times
  timei <- read_dimensions_maps(nci)$time
  time  <- c(time, timei)
  
  # Loop over times
  for (i in 1:length(timei)) {
    
    buffer      <- read_maps(nci, i)$fronts
    buffer      <- ! is.na(buffer)
    climatology <- climatology + buffer
    
  } # i
  
  # Close input file
  close_maps(nci)
  
} # infile

# Compute climatology
climatology <- climatology / length(time)

# Make climatology attributes
interval           <- time[2] - time[1]
climatology_time   <- median(c(time, time[length(time)] + interval))
climatology_bounds <- c(time[1], time[length(time)] + interval)

# Define dimensions
lon_dim  <- ncdim_def("longitude", "degrees_east" , lon, longname = "Longitude")
lat_dim  <- ncdim_def("latitude" , "degrees_north", lat, longname = "Latitude")
time_dim <- ncdim_def("time", time_units, climatology_time, 
                     unlim    = TRUE, 
                     calendar = calendar, 
                     longname = "Time")
nv_dim   <- ncdim_def("nv", "", 1:2, create_dimvar = FALSE)

# Define variables
fronts_var <- ncvar_def("fronts", "incidence", list(lon_dim, lat_dim, time_dim),
                       missval  = 9.9692099683868690e+36,
                       longname = "Fronts", 
                       prec     = "float")
bounds_var <- ncvar_def("climatology_bounds", "", list(nv_dim, time_dim), 
                       prec = "double")

# Create netCDF file
nco <- nc_create(outfile, list(bounds_var, fronts_var))

# Description
ncatt_put(nco, 0, "Conventions", "CF-1.9")
ncatt_put(nco, 0, "title", "Climatology of meteorological fronts")
ncatt_put(nco, 0, "source", "https://github.com/phil-sansom/front_id/tree/v0.9.2")
ncatt_put(nco, 0, "references", "Hewson, T.D. (1998), Objective fronts. Met. Apps, 5: 37-65. https://doi.org/10.1017/S1350482798000553")

# History
attnames <- names(attributes)
if ("history" %in% attnames) {
  ncatt_put(nco, 0, "history", paste(history, attributes$history, sep = "\n"))
} else {
  ncatt_put(nco, 0, "history", history)
}

# Standard names
ncatt_put(nco, "longitude", "standard_name", "longitude")
ncatt_put(nco, "latitude" , "standard_name", "latitude")
ncatt_put(nco, "time"     , "standard_name", "time")

# Write axes
ncatt_put(nco, "longitude", "standard_name", "X", prec = "text")
ncatt_put(nco, "latitude" , "standard_name", "Y", prec = "text")
ncatt_put(nco, "time"     , "standard_name", "T", prec = "text")

# Write global attributes
mask <- attnames %in% c("Conventions", "title", "source", "references", "history")
attnames <- attnames[!mask]
for (attname in attnames) {
  ncatt_put(nco, 0, attname, attributes[[attname]])
}

# Write attributes
ncatt_put(nco, "time", "climatology", "climatology_bounds")
ncatt_put(nco, "fronts", "cell_methods", 
          paste0("time: mean (interval: ", interval, " ", units, ")"))

# Write data
ncvar_put(nco, "climatology_bounds", climatology_bounds)
ncvar_put(nco, "fronts", climatology)

# Close file
nc_close(nco)
