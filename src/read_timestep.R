# Load libraries
library(ncdf4)

# Load source
source("src/get_dimensions.R")
source("src/invert_lat.R")

read_step <- function(nc, varname, x0, xn, y0, yn, level, t) {
  
  if (level == 0) {
    if (t == 0) {
      z <- ncvar_get(nc, varname, start = c(x0, y0),
                     count = c(xn, yn))
    } else {
      z <- ncvar_get(nc, varname, start = c(x0, y0, t),
                     count = c(xn, yn, 1))
    } # time
  } else {
    if (t == 0) {
      z <- ncvar_get(nc, varname, start = c(x0, y0, level),
                     count = c(xn, yn, 1))
    } else {
      z <- ncvar_get(nc, varname, start = c(x0, y0, level, t),
                     count = c(xn, yn, 1, 1))
    } # time
  } # level
  
  # Return data
  return(z)
  
} # read_step


read_timestep <- function(nc, varname, domain, t = 0, level = 0, tol = 0.001) {
  
  # Extract dimensions
  dims <- get_dimensions(nc, varname, t != 0, tol = tol)
  lon  <- dims$lon
  lat  <- dims$lat
  nlon <- length(lon)
  nlat <- length(lat)
  dlon <- (max(lon) - min(lon)) / (length(lon) - 1)
  dlat <- (max(lat) - min(lat)) / (length(lat) - 1)
  
  # If not expecting a time dimension, check whether one was found
  if (t == 0) {
    if (length(dims$time) == 1) {
      # If time dimension found but of length 1, then fine
      t <- 1
    } else if (1 < length(dims$time)) {
      # If time dimension has length > 1, then stop
      stop(paste0("Time dimension of length > 1 found for ", varname, " in ",
                  nc$filename, ", length should be 0 or 1"))
    }
  }
  
  # Transform weird extents
  if (180 < min(lon) || abs(180 - min(lon)) < tol) {
    lon <- lon - 360
  } else if (min(lon) < -180 && (max(lon) < 0 || abs(max(lon) - 0) < tol)) {
    lon <- lon + 360
  }
  
  # Get longitudinal extent
  minlon <- min(lon)
  maxlon <- max(lon)
  
  # Restrict y-axis
  lat_mask  <- which(domain$minlat - tol <= lat & lat <= domain$maxlat + tol)
  lat_start <- min(lat_mask)
  lat_end   <- max(lat_mask)
  lat_count <- lat_end - lat_start + 1
  y <- lat[lat_start:lat_end]
  
  if (maxlon - minlon < 360 - dlon - tol) {
    
    # Data extent is less than global, so just need to check domain
    # Assuming already used compare_domains to check compatibility
    
    if (domain$minlon < minlon - dlon / 2 - tol) {
      
      # If the domain exceeds the western extent of the data, then shift east
      lon_mask <- which(domain$minlon + 360 - tol <= lon & 
                          lon <= domain$maxlon + 360 + tol)
      
    } else if (maxlon + dlon / 2 + tol < domain$maxlon) {
      
      # If the domain exceeds the eastern extent of the data, then shift west
      lon_mask <- which(domain$minlon - 360 - tol <= lon & 
                          lon <= domain$maxlon - 360 + tol)
      
    } else {
      
      # Domain is inside the extent of the data
      lon_mask <- which(domain$minlon - tol <= lon & lon <= domain$maxlon + tol)
      
    }
    
    # Read 
    lon_start <- min(lon_mask)
    lon_end   <- max(lon_mask)
    lon_count <- lon_end - lon_start + 1
    x <- lon[lon_start:lon_end]
    z <- read_step(nc, varname, lon_start, lon_count, lat_start, lat_count, 
                   level, t)
    
  } else {
    
    # Extent is global
    
    if ((minlon < domain$minlon || abs(minlon - domain$minlon) < tol) && 
        (domain$maxlon < maxlon || abs(maxlon - domain$maxlon) < tol)) {
      
      # If domain is entirely within extent of data then one part read, ...
      lon_mask  <- which(domain$minlon - tol <= lon & lon <= domain$maxlon + tol)
      lon_start <- min(lon_mask)
      lon_end   <- max(lon_mask)
      lon_count <- lon_end - lon_start + 1
      x <- lon[lon_start:lon_end]
      z <- read_step(nc, varname, lon_start, lon_count, lat_start, lat_count, 
                     level, t)

    } else {
      
      # ...otherwise read in two parts
      if (domain$minlon < minlon - dlon / 2 - tol) {
        
        # Domain exceeds western edge of data
        
        # Part 1
        lon_mask  <- which(domain$minlon + 360 - tol <= lon)
        lon_start <- min(lon_mask)
        lon_end   <- nlon
        lon_count <- lon_end - lon_start + 1
        x1 <- lon[lon_start:lon_end] - 360
        z1 <- read_step(nc, varname, lon_start, lon_count, lat_start, lat_count,
                        level, t)
        
        # Part 2
        lon_mask  <- which(lon <= domain$maxlon + tol)
        lon_start <- 1
        lon_end   <- max(lon_mask)
        lon_count <- lon_end - lon_start + 1
        x2 <- lon[lon_start:lon_end]
        z2 <- read_step(nc, varname, lon_start, lon_count, lat_start, lat_count,
                        level, t)
        
      } else {
        
        # Domain exceeds eastern edge of data
        
        # Part 1
        lon_mask  <- which(domain$minlon - tol <= lon)
        lon_start <- 1
        lon_end   <- max(lon_mask)
        lon_count <- lon_end - lon_start + 1
        x1 <- lon[lon_start:lon_end]
        z1 <- read_step(nc, varname, lon_start, lon_count, lat_start, lat_count,
                        level, t)
        
        # Part 2
        lon_mask  <- which(lon <= domain$maxlon - 360 + tol)
        lon_start <- min(lon_mask)
        lon_end   <- nlon
        lon_count <- lon_end - lon_start + 1
        x2 <- lon[lon_start:lon_end] + 360
        z2 <- read_step(nc, varname, lon_start, lon_count, 
                        lat_start, lat_count, level, t)
        
      } # domain$minlon < minlon
      
      # Join sections
      x <- c(x1, x2)
      z <- rbind(z1, z2)
      
      # Check for duplicated longitudes
      nx= length(x)
      if (abs(x[1] - (x[nx] - 360)) < tol) {
        x <- x[-nx]
        z <- z[-nx, ]
      }
      
    } # minlon <= domain$minlon & domain$maxlon <= maxlon
    
  }
  
  # Invert latitude, if required
  if (lat[2] < lat[1]) {
    z <- invert_lat(z)
    y <- rev(y)
  }
  
  # Add offset
  if (domain$minlon < -180 - tol || +180 + tol < domain$maxlon) {
    offset <- (domain$minlon + domain$maxlon) / 2
    x <- x - offset
  } else {
    offset <- 0
  }
  
  # Return data
  list(x = x, y = y, z = z, offset = offset)
  
} # read_timestep
