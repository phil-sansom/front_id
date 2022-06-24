#!/usr/bin/env -S Rscript --vanilla

# Load libraries
library(optparse)
library(ncdf4)

# Load source
source("src/get_dimensions.R")
source("src/get_varname.R")
source("src/lonflip.R")
source("src/domains.R")

# Input parameters ------------------------------------------------------------ 

# Optional arguments
option_list <- list(
  make_option(c("--wfile","-w"), action = "store", type = "character",
              dest = "wfile",
              help = "Wet-bulb potential temperature file"),
  make_option(c("--tfile","-t"), action = "store", type = "character",
              dest = "tfile",
              help = "Air temperature file"),
  make_option(c("--qfile","-q"), action = "store", type = "character",
              dest = "qfile",
              help = "Specific humidity file [default: "),
  make_option(c("--rfile","-r"), action = "store", type = "character",
              dest = "rfile",
              help = "Relative humidity file"),
  make_option("--wfile-ref", action = "store", type = "character",
              dest = "wfile0",
              help = "Reference wet-bulb potential temperature file"),
  make_option("--tfile-ref", action = "store", type = "character",
              dest = "tfile0", default = "data/t850/erai_t850_200001.nc",
              help = "Reference air temperature file [default: data/t850/erai_t850_200001.nc]"),
  make_option("--qfile-ref", action = "store", type = "character",
              dest = "qfile0", default = "data/q850/erai_q850_200001.nc",
              help = "Reference specific humidity file [default: data/q850/erai_q850_200001.nc]"),
  make_option("--rfile-ref", action = "store", type = "character",
              dest = "rfile0",
              help = "Reference relative humidity file"),
  make_option("--nsmooth", action = "store", type = "integer", default = 8,
              help = "Number of smoothing passes to use for reference analysis [default: 8]"),
  make_option("--pressure", action = "store", type = "double", default = 85000,
              help = "Pressure level the input data are measured on (Pa) [default: 85000]"),
  make_option("--domain", action = "store", type = "character",
              help = "Limit the domain for analysis (lon1,lon2,lat1,lat2)"),
  make_option("--level-ref", action = "store", type = "integer", default = 0,
              dest = "level0",
              help = "Level number to use if reference input files contain multiple pressure levels"),
  make_option("--wname-ref", action = "store", type = "character", dest = "wname0",
              help = "Variable name in reference wet-bulb potential temperature file"),
  make_option("--tname-ref", action = "store", type = "character", dest = "tname0",
              help = "Variable name in reference air temperature file"),
  make_option("--qname-ref", action = "store", type = "character", dest = "qname0", 
              help = "Variable name in reference specific humidity file"),
  make_option("--rname-ref", action = "store", type = "character", dest = "rname0",
              help = "Variable name in reference relative humidity file"),
  make_option("--level", action = "store", type = "integer", default = 0,
              help = "Level number to use if input files contain multiple pressure levels"),
  make_option("--wname", action = "store", type = "character",
              help = "Variable name in wet-bulb potential temperature file"),
  make_option("--tname", action = "store", type = "character",
              help = "Variable name in air temperature file"),
  make_option("--qname", action = "store", type = "character", 
              help = "Variable name in specific humidity file"),
  make_option("--rname", action = "store", type = "character",
              help = "Variable name in relative humidity file"),
  make_option("--quantile", action = "store", type = "double", default = 0.25,
              help = "Quantile to match [default: 0.25]"),
  make_option("--quantity", action = "store", type = "character", 
              default = "tfp", 
              help = "Quantity to match (tfp/maggrad) [default: tfp]"),
  make_option("--lower", action = "store", type = "integer", default = 0,
              help = "Lower end of the interval to be searched [default: 0]"),
  make_option("--upper", action = "store", type = "integer",
              help = "Upper end of the interval to be searched"),
  make_option("--opttol", action = "store", type = "double", default = 1,
              help = "The desired accuracy for optimization [default: 1]"),
  make_option("--tol", action = "store", type = "double", default = 0.001,
              help = "Tolerance for coordinate checking (degrees) [default: 0.001]"),
  make_option("--fourthorder", action = "store_true", type = "logical", 
              default = FALSE,
              help = "Use fourth rather than second order accurate finite differences"),
  make_option("--includepoles", action = "store_true", type = "logical", 
              default = FALSE,
              help = "Include the -90N and +90N if they exist, excluded by default")
)

# Argument parser
parser <- OptionParser(usage = paste("Usage:",
                                     "\t%prog [OPTION]... --wfile=WFILE",
                                     "\t%prog [OPTION]... --tfile=TFILE --qfile=QFILE",
                                     "\t%prog [OPTION]... --tfile=TFILE --rfile=RFILE",
                                     sep = "\n"),
                       option_list = option_list,
                       description = "Determine the number of smoothing cycles required to make the distribution of the thermal front parameter in a new model comparable to a reference model.")

# Parse arguments
args <- commandArgs(TRUE)
opts <- parse_args(parser, args = args, positional_arguments = FALSE)
if (! xor(xor(exists("qfile0", opts), exists("rfile0", opts)) && exists("tfile0", opts),
          exists("wfile0", opts))) {
  stop("Either --wfile-ref alone, or --tfile-ref and one of --qfile-ref and --rfile-ref must be specified.")
}
if (! xor(xor(exists("qfile", opts), exists("rfile", opts)) && exists("tfile", opts),
          exists("wfile", opts))) {
  stop("Either --wfile alone, or --tfile and one of --qfile and --rfile must be specified.")
}
if (! opts$quantity %in% c("tfp", "maggrad")) {
  stop("--quantity must be either tfp or maggrad")
}


# Check domain ----------------------------------------------------------------

# Open connections and get dimensions
if (exists("wfile0", opts)) {
  nc0 <- nc_open(opts$wfile0)
  if (! exists("wname0", opts)) {
    opts$wname0 <- get_varname(nc0, "wet_bulb_potential_temperature")
  }
  dims0 <- get_dimensions(nc0, opts$wname0, tol = opts$tol)
} else {
  nc0 <- nc_open(opts$tfile0)
  if (! exists("tname0", opts)) {
    opts$tname0 <- get_varname(nc0, "air_temperature")
  }
  dims0 <- get_dimensions(nc0, opts$tname, tol = opts$tol)
} # wfile0
if (exists("wfile", opts)) {
  nc1 <- nc_open(opts$wfile)
  if (! exists("wname", opts)) {
    opts$wname <- get_varname(nc1, "wet_bulb_potential_temperature")
  }
  dims1 <- get_dimensions(nc1, opts$wname, tol = opts$tol)
} else {
  nc1 <- nc_open(opts$tfile)
  if (! exists("tname", opts)) {
    opts$tname <- get_varname(nc1, "air_temperature")
  }
  dims1 <- get_dimensions(nc1, opts$tname, tol = opts$tol)
} # wfile

# Set upper limit on smoothing search
if (! exists("upper", opts)) {
  opts$upper <- max(2 * ceiling(5 / abs(dims1$lat[2] - dims1$lat[1])^2 - 1),
                    opts$nsmooth)
}

# Extract dimensions
lon0 <- dims0$lon
lat0 <- dims0$lat
lon1 <- dims1$lon
lat1 <- dims1$lat
if (any(lon0 > 180)) {
  lon0 <- lonflip(matrix(0, length(lon0), length(lat0)), lon0)$lon
}
if (any(lon1 > 180)) {
  lon1 <- lonflip(matrix(0, length(lon1), length(lat1)), lon1)$lon
}
dlon0 <- (max(lon0) - min(lon0)) / (length(lon0) - 1)
dlon1 <- (max(lon1) - min(lon1)) / (length(lon1) - 1)

# Close connections
nc_close(nc0)
nc_close(nc1)

# Check domain
if (exists("domain", opts)) {
  # If a domain is defined, parse it, check it and compare it to the data
  domain <- parse_domain(opts$domain)
  domain <- check_domain(lon0, lat0, domain, 
                         exclude_poles = ! opts$includepoles, 
                         tol           = opts$tol)
  domain <- compare_domains(lon0, lat0, domain, tol = opts$tol)
  domain <- compare_domains(lon1, lat1, domain, tol = opts$tol)
} else {
  # If no domain is defined, set it equal to the extent of the data
  domain0 <- set_domain(lon0, lat0)
  domain1 <- set_domain(lon1, lat1)
  domain0 <- check_domain(lon0, lat0, domain0, 
                          exclude_poles = ! opts$includepoles, 
                          tol           = opts$tol)
  domain1 <- check_domain(lon1, lat1, domain1, 
                          exclude_poles = ! opts$includepoles, 
                          tol           = opts$tol)
  domain <- list(
    minlon = max(domain0$minlon, domain1$minlon),
    maxlon = min(domain0$maxlon, domain1$maxlon),
    minlat = max(domain0$minlat, domain1$minlat),
    maxlat = min(domain0$maxlat, domain1$maxlat)
  )
}

# Print domain
domain_string <- paste(domain$minlon, domain$maxlon,
                       domain$minlat, domain$maxlat,
                       sep = ",")
print(paste0("Domain is ",
             round(domain$minlon, 3), "E -", round(domain$maxlon, 3), "E",
             " and ",
             round(domain$minlat, 3), "N -", round(domain$maxlat, 3), "N"))




# Compute reference analysis --------------------------------------------------

print("Computing reference analysis...")

# Make command
command0 <- paste0("./identify_fronts.R", " --diagnostics", " --nofronts",
                   " --nsmooth=", opts$nsmooth, " --pressure=", opts$pressure,
                   " --domain=", domain.string)
if (exists("level0", opts))
  command0 <- paste0(command0, " --level=", opts$level0)
if (exists("wname0", opts))
  command0 <- paste0(command0, " --wname=", opts$wname0)
if (exists("tname0", opts))
  command0 <- paste0(command0, " --tname=", opts$tname0)
if (exists("qname0", opts))
  command0 <- paste0(command0, " --qname=", opts$qname0)
if (exists("rname0", opts)) 
  command0 <- paste0(command0, " --rname=", opts$rname0)
if (exists("wfile0", opts))
  command0 <- paste0(command0, " --wfile=", opts$wfile0)
if (exists("tfile0", opts)) 
  command0 <- paste0(command0, " --tfile=", opts$tfile0)
if (exists("qfile0", opts)) 
  command0 <- paste0(command0, " --qfile=", opts$qfile0)
if (exists("rfile0", opts)) 
  command0 <- paste0(command0, " --rfile=", opts$rfile0)
if (opts$fourthorder)  
  command0 <- paste0(command0," --fourthorder")
if (opts$includepoles) 
  command0 <- paste0(command0," --includepoles")
command0 <- paste0(command0," reference.nc")

# Compute reference analysis
system(command0)

# Load reference analysis
nc     <- nc_open("reference_diagnostics.nc")
buffer <- ncvar_get(nc, opts$quantity)
target <- quantile(buffer, opts$quantile, na.rm = TRUE)
print(paste("Target:", sprintf("%8.2e", target), "Km^-3"))
nc_close(nc)


# Optimization ----------------------------------------------------------------

print(paste0("Optimizing target analysis over [", 
             opts$lower, ",", opts$upper, "]..."))

f <- function(x, target, opts) {
  
  print(paste(round(x), "smoothing cycles"))
  
  # Create command
  command1 <- paste0("./identify_fronts.R", " --diagnostics", " --nofronts",
                     " --nsmooth=", round(x), " --pressure=", opts$pressure,
                     " --domain=", domain_string)
  if (exists("level", opts))
    command1 <- paste0(command1, " --level=", opts$level)
  if (exists("wname", opts))
    command1 <- paste0(command1, " --wname=", opts$wname)
  if (exists("tname", opts))
    command1 <- paste0(command1, " --tname=", opts$tname)
  if (exists("qname", opts))
    command1 <- paste0(command1, " --qname=", opts$qname)
  if (exists("rname", opts))
    command1 <- paste0(command1, " --rname=", opts$rname)
  if (exists("wfile", opts))
    command1 <- paste0(command1, " --wfile=", opts$wfile)
  if (exists("tfile", opts))
    command1 <- paste0(command1, " --tfile=", opts$tfile)
  if (exists("qfile", opts))
    command1 <- paste0(command1, " --qfile=", opts$qfile)
  if (exists("rfile", opts))
    command1 <- paste0(command1, " --rfile=", opts$rfile)
  if (opts$fourthorder)
    command1 <- paste0(command1, " --fourthorder")
  if (opts$includepoles)
    command1 <- paste0(command1, " --includepoles")
  command1 <- paste0(command1, " target.nc")
  
  # Compute reference analysis
  system(command1)
  
  # Load reference analysis
  nc     <- nc_open("target_diagnostics.nc")
  buffer <- ncvar_get(nc, opts$quantity)
  z      <- quantile(buffer, opts$quantile, na.rm = TRUE)
  print(paste("Current:", sprintf("%8.2e", z), "Km^-3"))
  nc_close(nc)
  
  # Return result
  abs(target - z)
  
} # f

nsmooth <- optimize(f, c(opts$lower - 0.5, opts$upper + 0.5), 
                    target = target, 
                    opts   = opts, 
                    tol    = opts$opttol)

print(paste(round(nsmooth$minimum), "smoothing cycles"))

write(round(nsmooth$minimum), "nsmooth.txt")

# Warn if close to upper bound
if (opts$upper - round(nsmooth$minimum) <= 1) {
  warning("Result is close to the upper limit, consider rerunning with a higher limit")
}

# Garbage collection
file.remove("reference_diagnostics.nc","target_diagnostics.nc")
