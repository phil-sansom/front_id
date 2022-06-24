#!/usr/bin/env -S Rscript --vanilla

# Load libraries
library(optparse)

# Load source
source("src/gcd.R")
source("src/history.R")
source("src/maps_io.R")
radius <- 6378137

# Optional arguments
option_list <- list(
  make_option(c("--threshold","-t"), action = "store", type = "double",
              default = 251, 
              help = "Radius to expand fronts by (km) [default: 251]")
)

# Argument parser
parser <- OptionParser(usage = "Usage: %prog [OPTION]... INFILE OUTFILE",
                       option_list = option_list,
                       description = "Expand meteorological fronts.")

# Parse arguments
args      <- commandArgs(TRUE)
history   <- make_history("./expand_fronts.R", args)
argv      <- parse_args(parser, args = args, positional_arguments = 2)
opts      <- argv$options
args      <- argv$args
infile    <- args[1]
outfile   <- args[2]
threshold <- opts$threshold

# Open input file
nci <- open_maps(infile)

# Read attributes
attributes           <- read_attributes_maps(nci)
attributes$expansion <- threshold

# Read dimensions
dimensions <- read_dimensions_maps(nci)
lon  <- dimensions$longitude
lat  <- dimensions$latitude
time <- dimensions$time
nx <- length(lon)
ny <- length(lat)
nt <- length(time)

# Assume regular grid
dx <- (max(lon) - min(lon)) / (nx - 1)
dy <- (max(lat) - min(lat)) / (ny - 1)

# Total x domain size
nxt <- as.integer(round(360 / dx))

dk  <- radius * pi / 180 / 1000  # 1 degree in km at equator
dky <- dk * dy                   # y grid distance in km
dny <- floor(threshold / dky)    # Number of grid boxes to search in y direction

# Create stencil library
stencils <- list()
for (i in 1:ny) {
  
  y1 <- min(i + dny, ny)
  y0 <- max(i - dny, 1)
  
  dkx <- dx * min(cos(pi * lat[y1] / 180), cos(pi * lat[y0] / 180)) * dk
  dnx <- floor(threshold / dkx)  # Number of grid boxes to search in x direction
  
  if (nxt / 2 < nx && nx < nxt) {
    dnx <- min(dnx, nx)
  } else {
    dnx <- min(dnx, floor(nxt / 2))
  }
  x0   <- -dnx
  x1   <- +dnx
  lon1 <- seq(x0 * dx, x1 * dx, dx)
  
  grid  <- as.matrix(expand.grid(x0:x1, y0:y1))
  dists <- gcd(c(0, lat[i]), cbind(lon1[grid[, 1] + dnx + 1], lat[grid[, 2]]),
               r = radius) / 1000
  
  stencils[[i]] <- grid[dists <= threshold, , drop = FALSE]
  
} # i

# Create output file
nco <- create_maps(outfile, lon, lat, dimensions$time_units,
                   dimensions$calendar, attributes, 
                   fields  = FALSE,
                   title   = paste("Expanded", tolower(attributes$title)),
                   history = history, frspeed = FALSE)

# Loop over times
for (i in 1:nt) {
  
  # Initialize storage
  output <- matrix(NA, nx, ny)
  
  # Load data
  input <- read_maps(nci, i)$fronts
  
  # Identify points for expansion
  indices <- which(! is.na(input), arr.ind = TRUE)
  
  if (nrow(indices) > 0) {
    
    # Loop over indices
    for (j in 1:nrow(indices)) {
      
      x <- indices[j, 1]
      y <- indices[j, 2]
      
      # Use stencil to expand object
      stencil <- stencils[[y]]
      stencil[, 1] <- stencil[, 1] + x
      if (nx < nxt) {
        stencil <- stencil[1 <= stencil[, 1] & stencil[, 1] <= nx, ]
      } else {
        stencil[, 1] <- (stencil[, 1] - 1) %% nx + 1
      }
      output[stencil] <- 1
      
    } # i
    
  } # nrow(indices > 0)
  
  # Write data
  nco <- write_maps(nco, time[i], list(fronts = output))
  
} # i

# Close files
close_maps(nci)
close_maps(nco)
