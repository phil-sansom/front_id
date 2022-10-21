#!/usr/bin/env -S Rscript --vanilla

# Preamble --------------------------------------------------------------------

# Load libraries
library(optparse)
library(ncdf4)

# Load source
source("src/compare_dimensions.R")
source("src/compute_relative_humidity.R")
source("src/compute_tfp.R")
source("src/compute_thetaw.R")
source("src/diagnostics.R")
source("src/domains.R")
source("src/fronts_io.R")
source("src/get_dimensions.R")
source("src/get_varname.R")
source("src/history.R")
source("src/identify_fronts.R")
source("src/maps_io.R")
source("src/read_timestep.R")


# Parse input parameters ------------------------------------------------------

# Optional arguments
option_list <- list(
  make_option(c("--wfile","-w"), action = "store", type = "character",
              help = "Wet-bulb potential temperature file"),
  make_option(c("--tfile","-t"), action = "store", type = "character",
              help = "Air temperature file"),
  make_option(c("--qfile","-q"), action = "store", type = "character",
              help = "Specific humidity file"),
  make_option(c("--rfile","-r"), action = "store", type = "character",
              help = "Relative humidity file"),
  make_option("--nsmooth", action = "store", type = "integer", default = 8,
              help = "Number of smoothing passes to use [default: 8]"),
  make_option("--thresh_t", action = "store", type = "double", default = -1.5e-11,
              help = "Thermal front parameter threshold (K/m^2) [default: -1.5e-11]"),
  make_option("--thresh_g", action = "store", type = "double", default = 7.3e-6,
              help = "Magnitude of gradient threshold (K/m) [default: 7.3e-6]"),
  make_option("--minlength", action = "store", type = "double", default = 250,
              help = "Minimum front length (km) [default: 250]"),
  make_option("--searchrad", action = "store", type = "double", default = 170,
              help = "Search radius for contour/line joining (km) [default: 170]"),
  make_option("--pressure", action = "store", type = "double", default = 85000,
              help = "Pressure level the input data are measured on (Pa) [default: 85000]"),
  make_option("--tol", action = "store", type = "double", default = 0.001,
              help = "Tolerance for coordinate checking (degrees) [default: 0.001]"),
  make_option("--level", action = "store", type = "integer", default = 0,
              help = "Level number to use if input files contain multiple pressure levels"),
  make_option("--domain", action = "store", type = "character",
              help = "Limit the domain for analysis (lon1,lon2,lat1,lat2)"),
  make_option("--wname", action = "store", type = "character", 
              help = "Variable name in wet-bulb potential temperature file"),
  make_option("--tname", action = "store", type = "character", 
              help = "Variable name in air temperature file"),
  make_option("--qname", action = "store", type = "character",
              help = "Variable name in specific humidity file"),
  make_option("--rname", action = "store", type = "character", 
              help = "Variable name in relative humidity file"),
  make_option(c("--ufile","-u"), action = "store", type = "character",
              help = "Eastward wind file"),
  make_option(c("--vfile","-v"), action = "store", type = "character",
              help = "Northward wind file"),
  make_option("--uname", action = "store", type = "character",
              help = "Variable name in zonal wind file"),
  make_option("--vname", action = "store", type = "character",
              help = "Variable name in meridional wind file"),
  make_option("--diagnostics", action = "store_true", type = "logical",
              default = FALSE, help = "Output diagnostic variables"),
  make_option("--nofronts", action = "store_false", type = "logical",
              dest = "findfronts", default = TRUE,
              help = "Do not identify or output fronts"),
  make_option("--fourthorder", action = "store_true", type = "logical", 
              default = FALSE,
              help = "Use fourth rather than second order accurate finite differences"),
  make_option("--includepoles", action = "store_true", type = "logical", 
              default = FALSE,
              help = "Include the -90N and +90N if they exist, excluded by default")
)

# Argument parser
parser <- OptionParser(usage = "Usage: \n\t%prog [OPTION]... --wfile=WFILE OUTFILE\n\t%prog [OPTION]... --tfile=TFILE --qfile=QFILE OUTFILE\n\t%prog [OPTION]... --tfile=TFILE --rfile=RFILE OUTFILE",
                       option_list = option_list,
                       description = "Identify meteorological fronts.\n\nOperands:\n\tOUTFILE\n\t\tFile to write output to")

# Parse arguments
args    <- commandArgs(TRUE)
history <- make_history("./identify_fronts.R", args)
argv    <- parse_args(parser, args = args, positional_arguments = 1)
opts    <- argv$options
args    <- argv$args
outfile <- as.character(args)

# Set options
opts$radius <- 6378137
if (exists("rfile", opts)) {
  opts$calcr <- FALSE
} else {
  opts$calcr <- TRUE
}
if (exists("wfile", opts)) {
  opts$calcthetaw <- FALSE
} else {
  opts$calcthetaw <- TRUE
}
if (! xor(xor(exists("qfile", opts), exists("rfile", opts)) && exists("tfile", opts),
          exists("wfile", opts))) {
  stop("Either --wfile alone, or --tfile and one of --qfile and --rfile must be specified.")
}
if (exists("ufile", opts) && exists("vfile", opts)) {
  opts$frspeed <- TRUE
} else {
  opts$frspeed <- FALSE
}
if (xor(exists("ufile", opts),exists("vfile", opts))) {
  stop("Both --ufile and --vfile must be specified to compute front speed.")
}
if (opts$thresh_t > 0) {
  stop("--thresh_t must be less than or equal to 0")
}
if (opts$thresh_g < 0) {
  stop("--thresh_g must be greater than or equal to 0")
}
if (opts$nsmooth < 0) {
  stop("--nsmooth must be greater than or equal to 0")
}
if (opts$minlength < 0) {
  stop("--minlength must be greater than or equal to 0")
}
if (opts$searchrad < 0) {
  stop("--searchrad must be greater than or equal to 0")
}
if (opts$pressure < 0) {
  stop("--pressure must be greater than or equal to 0")
}


# Open input file connections -------------------------------------------------

# Open thermal front parameter files
if (opts$calcthetaw) {
  if (! file.exists(opts$tfile)) {
    stop(paste(opts$tfile, "does not exist"))
  }
  tf <- nc_open(opts$tfile)
  if (! exists("tname", opts)) {
    opts$tname <- get_varname(tf, "air_temperature")
  }
  if (opts$calcr) {
    if (! file.exists(opts$qfile)) {
      stop(paste(opts$qfile, "does not exist"))
    }
    qf <- nc_open(opts$qfile)
    if (! exists("qname", opts)) {
      opts$qname <- get_varname(qf, "specific_humidity")
    }
  } else {
    if (! file.exists(opts$rfile)) {
      stop(paste(opts$rfile, "does not exist"))
    }
    rf <- nc_open(opts$rfile)
    if (! exists("rname", opts)) {
      opts$rname <- get_varname(rf, "relative_humidity")
    }
  } # calcr
} else {
  if (! file.exists(opts$wfile)) {
    stop(paste(opts$wfile, "does not exist"))
  }
  wf <- nc_open(opts$wfile)
  if (! exists("wname", opts)) {
    opts$wname <- get_varname(wf, "wet_bulb_potential_temperature")
  }
} # calcthetaw

# Open wind files
if (opts$frspeed) {
  if (! file.exists(opts$ufile)) {
    stop(paste(opts$ufile, "does not exist"))
  }
  uf <- nc_open(opts$ufile)
  if (! exists("uname", opts)) {
    opts$uname <- get_varname(uf, "eastward_wind")
  }
  if (! file.exists(opts$vfile)) {
    stop(paste(opts$vfile, "does not exist"))
  }
  vf <- nc_open(opts$vfile)
  if (! exists("vname", opts)) {
    opts$vname <- get_varname(vf, "northward_wind")
  }
} # frspeed


# Check input files -----------------------------------------------------------

# Check dimensions in thermal front parameter files
if (opts$calcthetaw) {
  if (tf$var[[opts$tname]]$ndims < 3) {
    stop (paste(tf$filename, "only has 2 dimensions"))
  }
  if (tf$var[[opts$tname]]$ndims == 4 && opts$level == 0) {
    stop (paste(tf$filename, "has 4 dimensions, please specify model level using --level"))
  }
  dims <- get_dimensions(tf, opts$tname, tol = opts$tol)
  # Check dimensions in humidity files
  if (opts$calcr) {
    compare_dimensions(tf, opts$tname, qf, opts$qname, tol = opts$tol)
  } else {
    compare_dimensions(tf, opts$tname, rf, opts$rname, tol = opts$tol)
  } # calcr
} else {
  if (wf$var[[opts$wname]]$ndims < 3) {
    stop (paste(tf$filename, "only has 2 dimensions"))
  }
  if (wf$var[[opts$wname]]$ndims == 4 && opts$level == 0) {
    stop (paste(tf$filename, "has 4 dimensions, please specify model level using --level"))
  }
  dims <- get_dimensions(wf, opts$wname, tol = opts$tol)
} # calcthetaw

# Check dimensions in wind files
if (opts$frspeed) {
  if (opts$calcthetaw) {
    compare_dimensions(tf, opts$tname, uf, opts$uname, tol = opts$tol)
    compare_dimensions(tf, opts$tname, vf, opts$vname, tol = opts$tol)
  } else {
    compare_dimensions(wf, opts$wname, uf, opts$uname, tol = opts$tol)
    compare_dimensions(wf, opts$wname, vf, opts$vname, tol = opts$tol)
  } # calcthetaw
} # frspeed


# Extract raw dimensions ------------------------------------------------------

time <- dims$time
lon0 <- dims$lon
lat0 <- dims$lat
time_units <- dims$time_units
calendar   <- dims$calendar
dlon <- (max(lon0) - min(lon0)) / (length(lon0) - 1)
dlat <- (max(lat0) - min(lat0)) / (length(lat0) - 1)


# Check domain ----------------------------------------------------------------

if (exists("domain", opts)) {
  # If a domain is defined, parse it, check it and compare it to the data
  domain <- parse_domain(opts$domain)
  domain <- check_domain(lon0, lat0, domain, 
                         exclude_poles = ! opts$includepoles, 
                         tol           = opts$tol)
  domain <- compare_domains(lon0, lat0, domain, tol = opts$tol)
} else {
  # If no domain is defined, set it equal to the extent of the data
  domain <- set_domain(lon0, lat0)
  domain <- check_domain(lon0, lat0, domain, 
                         exclude_poles = ! opts$includepoles, 
                         tol           = opts$tol)
}

# Check if we need to wrap across the anti-meridian
opts$global <- 360 - dlon < domain$maxlon - domain$minlon ||
  abs((360 - dlon) - (domain$maxlon - domain$minlon)) < opts$tol


#  Get dimensions to be read --------------------------------------------------

if (opts$calcthetaw) {
  buffer <- read_timestep(tf, opts$tname, domain, 1, opts$level, 
                          tol = opts$tol)
} else {
  buffer <- read_timestep(wf, opts$wname, domain, 1, opts$level, 
                          tol = opts$tol)
} # calcthetaw
lon1 <- buffer$x
lat1 <- buffer$y
offset <- buffer$offset
rm(buffer)


# Interpolation ---------------------------------------------------------------

lon_offset <- lon1
lat_offset <- lat1

# Lon & lat without offset
lon <- lon_offset + offset
lat <- lat_offset

# Tighten domain to data
domain$minlon <- lon[min(which(domain$minlon - opts$tol < lon))]
domain$maxlon <- lon[max(which(lon < domain$maxlon + opts$tol))]
domain$minlat <- lat[min(which(domain$minlat - opts$tol < lat))]
domain$maxlat <- lat[max(which(lat < domain$maxlat + opts$tol))]


# Set internal options --------------------------------------------------------

# Set max contour length to a unfeasibly large value
if (opts$global) {
  options(max.contour.segments = 3 * length(lon_offset) * length(lat_offset))
} else {
  options(max.contour.segments =     length(lon_offset) * length(lat_offset))
}


# Create output files ---------------------------------------------------------

# Attributes for writing
attributes <- opts[c("pressure", "nsmooth", "thresh_g", "thresh_t",
                     "minlength", "searchrad", "fourthorder", "includepoles")]

# Transform grid parameters to CDO grid spec
attributes$xsize  <- length(lon)
attributes$xfirst <- domain$minlon
attributes$xinc   <- dlon
attributes$ysize  <- length(lat)
attributes$yfirst <- domain$minlat
attributes$yinc   <- dlat

# Create output file
if (opts$findfronts) {
  nco <- create_fronts(outfile, time_units, calendar, attributes, 
                       history = history, 
                       frspeed = opts$frspeed)
}

# Create diagnostic file
if (opts$diagnostics) {
  dfile <- paste0(tools::file_path_sans_ext(outfile), "_diagnostics.nc")
  ncd   <- create_diagnostics(dfile, lon, lat, time_units, calendar, attributes, 
                              frspeed = opts$frspeed)
}


# Loop over time steps --------------------------------------------------------

for (i in 1:length(time)) {
  
  # Read data
  if (opts$calcthetaw) {
    
    # Read air temperature
    temperature <- read_timestep(tf, opts$tname, domain, i, opts$level, 
                                 tol = opts$tol)$z
    
    if (opts$calcr) {
      
      # Read specific humidity
      specific_humidity <- read_timestep(qf, opts$qname, domain, i, opts$level, 
                                         tol = opts$tol)$z
      
    } else {
      
      # Read relative humidity
      relative_humidity <- read_timestep(rf, opts$rname, domain, i, opts$level, 
                                         tol = opts$tol)$z
      
    } # calcr
    
  } else {
    
    # Read wet-bulb potential temperature
    thetaw <- read_timestep(wf, opts$wname, domain, i, opts$level, 
                            tol = opts$tol)$z
    
  } # calcthetaw
  
  if (opts$frspeed) {
    
    # Read eastward wind
    eastward_wind  <- read_timestep(uf, opts$uname, domain, i, opts$level, 
                                    tol = opts$tol)$z
    
    # Read northward wind
    northward_wind <- read_timestep(vf, opts$vname, domain, i, opts$level, 
                                    tol = opts$tol)$z
    
  } else {
    
    eastward_wind  <- NULL
    northward_wind <- NULL
    
  } # frspeed
  
  # Compute wet-bulb potential temperature
  if (opts$calcthetaw) {
    
    # Compute relative humidity
    if (opts$calcr) {
      relative_humidity <- 
        compute_relative_humidity(temperature, specific_humidity, opts$pressure)
    }
    
    # Compute theta_w
    theta_w <- compute_thetaw(temperature, relative_humidity, 
                              opts$pressure / 100)
    
  } # calcthetaw
  
  # Compute thermal front parameter and derived fields
  tfp <- compute_tfp(lon_offset, lat_offset, 
                     theta_w, eastward_wind, northward_wind, 
                     opts)
  
  # Find fronts
  if (opts$findfronts) {
    
    fronts <- identify_fronts(lon_offset, lat_offset, 
                              tfp$frloc, tfp$tfp, tfp$maggrad, tfp$frspeed, 
                              opts)
    
    # Add back x offset
    if (length(fronts) > 0 && offset != 0) {
      for (i in 1:length(fronts)) {
        fronts[[i]]$x <- fronts[[i]]$x + offset
      }
    }
    
    # Write fronts
    nco <- write_fronts(nco, time[i], fronts)
    
  } # findfronts
  
  # Write diagnostics
  if (opts$diagnostics) {
    ncd <- write_maps(ncd, time[i], tfp)
  }
  
  # Garbage collection
  rm(theta_w)
  if (opts$calcthetaw) {
    rm(relative_humidity, temperature)
    if (opts$calcr) rm(specific_humidity)
  }
  if (opts$frspeed) rm(eastward_wind, northward_wind)
  
} # i


# Close connections -----------------------------------------------------------

if (opts$calcthetaw) {
  nc_close(tf)
  if (opts$calcr) {
    nc_close(qf)
  } else {
    nc_close(rf)
  }
} else {
  nc_close(wf)
}
if (opts$frspeed) {
  nc_close(uf)
  nc_close(vf)
}
if (opts$findfronts)  close_fronts(nco)
if (opts$diagnostics) close_maps(ncd)
