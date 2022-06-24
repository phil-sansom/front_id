# Load libraries
library(ncdf4)

open_fronts <- function(file, write = FALSE) {
  
  nc <- nc_open(file, write)
  
  if (nc$dim$time$len == 0) {
    nc$tt <- 0
    nc$nn <- 0
    nc$pp <- 0
  } else {
    nc$tt <- nc$dim$time$len
    nc$nn <- nc$dim$fronts$len
    nc$pp <- nc$dim$points$len
  }
  
  nc
  
} # open_fronts


read_attributes_fronts <- function(nc) {
  
  ncatt_get(nc, 0)
  
} # read_attributes_fronts


read_dimensions_fronts <- function(nc) {
  
  dimensions <- list()
  
  # Read ordinary dimensions
  dimids <- names(nc$dim)
  for (dimid in dimids) {
    if (nc$dim[[dimid]]$create_dimvar) {
      dimensions[[dimid]] <- nc$dim[[dimid]]$vals
    }
  }
  
  # Read sample dimensions
  varids <- names(nc$var)
  for (varid in varids) {
    buffer <- ncatt_get(nc, varid, "sample_dimension")
    if (buffer$hasatt) {
      dimensions[[varid]] <- ncvar_get(nc, varid)
    }
  }
  
  # Read time attributes
  dimensions$time_units <- nc$dim$time$units
  dimensions$calendar   <- nc$dim$time$calendar
  
  dimensions
  
} # read_dimensions_fronts


read_fronts <- function(nc, t) {
  
  # Get dimensions
  n_fronts <- ncvar_get(nc, "nfronts")
  
  # Do we need to read front speeds?
  read_speed <- exists("frspeed", nc$var)
  
  # Initialize storage
  fronts <- list()
  
  if (n_fronts[t] > 0) {
    
    # Get dimensions
    npoints <- ncvar_get(nc, "npoints")
    
    # Read whole time step then split into fronts
    # Slightly more complicated, but much faster than one front at a time
    
    # Determine start point and number of points to read
    if (t > 1) {
      ftm1 <- sum(n_fronts[1:(t - 1)])
      ps   <- sum(npoints[1:ftm1]) + 1
    } else {
      ftm1 <- 0
      ps   <- 1
    }
    np <- npoints[(ftm1 + 1):(ftm1 + n_fronts[t])]
    pc <- sum(np)
    
    # Read time step  
    lon <- ncvar_get(nc, "longitude", start = ps, count = pc)
    lat <- ncvar_get(nc, "latitude" , start = ps, count = pc)
    tfp <- ncvar_get(nc, "tfp"      , start = ps, count = pc)
    mag <- ncvar_get(nc, "maggrad"  , start = ps, count = pc)
    if (read_speed) {
      spd <- ncvar_get(nc, "frspeed", start = ps, count = pc)
    }
    
    # Extract fronts
    pa <- 1
    for (i in 1:n_fronts[t]) {
      mask <- pa:(pa + np[i] - 1)
      front <- list()
      front$x         <- lon[mask]
      front$y         <- lat[mask]
      front$tfp       <- tfp[mask]
      front$maggrad   <- mag[mask]
      if (read_speed) {
        front$frspeed <- spd[mask]
      }
      fronts[[i]] <- front
      pa <- pa + np[i]
    } # j
    
  } # n_fronts
  
  # Return fronts
  fronts
  
}


close_fronts <- function(nc) {
  
  nc_close(nc)
  
} # read_fronts


create_fronts <- function(file, time_units, calendar, attributes,
                          title = "Meteorological fronts", 
                          history = NULL, 
                          frspeed = TRUE) {
  
  # Define dimensions
  points_dim <- ncdim_def("points", "", 1L, 
                          unlim = TRUE, 
                          create_dimvar = FALSE)
  fronts_dim <- ncdim_def("fronts", "", 1L, 
                          unlim = TRUE, 
                          create_dimvar = FALSE)
  time_dim   <- ncdim_def("time", time_units, numeric(), 
                          unlim = TRUE, 
                          calendar = calendar, 
                          longname = "Time")
  
  # Sample dimensions
  nfronts_dim <- ncvar_def("nfronts", "", time_dim, 
                           missval = -32767,
                           longname = "Number of fronts at this time",
                           prec = "short")
  npoints_dim <- ncvar_def("npoints", "", fronts_dim, 
                           missval = -32767,
                           longname = "Number of points in this front",
                           prec = "short")
  
  # Define variables
  lon_var <- ncvar_def("longitude", "degrees_east", points_dim, 
                       missval = 9.9692099683868690e+36,  
                       longname = "Longitude", 
                       prec = "float")
  lat_var <- ncvar_def("latitude", "degrees_north", points_dim,
                       missval = 9.9692099683868690e+36, 
                       longname = "Latitude", 
                       prec = "float")
  tfp_var <- ncvar_def("tfp", "K/m^2", points_dim,
                       missval = 9.9692099683868690e+36, 
                       longname = "Thermal front parameter",
                       prec = "float")
  mag_var <- ncvar_def("maggrad", "K/m", points_dim,
                       missval = 9.9692099683868690e+36, 
                       longname = "Magnitude of gradient", 
                       prec = "float")
  field_names <- c("nfronts", "npoints", "longitude", "latitude", "tfp", "maggrad")
  vars <- list(nfronts_dim, npoints_dim, lon_var, lat_var, tfp_var, mag_var)
  
  if (frspeed) {
    spd_var <- ncvar_def("frspeed", "m/s", points_dim, 
                         missval = 9.9692099683868690e+36,
                         longname = "Front speed", 
                         prec = "float")
    field_names <- c(field_names, "frspeed")
    vars <- c(vars, list(spd_var))
  } # frspeed
  
  # Create netCDF file
  nc <- nc_create(file, vars)
  
  # Description
  ncatt_put(nc, 0, "Conventions", "CF-1.9")
  ncatt_put(nc, 0, "title", title)
  ncatt_put(nc, 0, "source", "https://github.com/phil-sansom/front_id/tree/v0.9.2")
  ncatt_put(nc, 0, "references", "Hewson, T.D. (1998), Objective fronts. Met. Apps, 5: 37-65. https://doi.org/10.1017/S1350482798000553")
  
  # History
  attnames <- names(attributes)
  if (is.null(history)) {
    if ("history" %in% attnames) {
      ncatt_put(nc, 0 , "history", attributes$history)
    }
  } else {
    if ("history" %in% attnames) {
      ncatt_put(nc, 0, "history", paste(history, attributes$history, sep = "\n"))
    } else {
      ncatt_put(nc, 0, "history", history)
    }
  }
  
  # Standard names
  ncatt_put(nc, "longitude", "standard_name", "longitude")
  ncatt_put(nc, "latitude" , "standard_name", "latitude")
  ncatt_put(nc, "time"     , "standard_name", "time")
  
  # Contiguous ragged array representation
  ncatt_put(nc, "nfronts", "sample_dimension", "fronts")
  ncatt_put(nc, "npoints", "sample_dimension", "points")
  
  # Write global attributes
  mask <- attnames %in% c("Conventions", "title", "source", "references", "history")
  attnames <- attnames[! mask]
  for (attname in attnames) {
    if (is.logical(attributes[[attname]])) {
      ncatt_put(nc, 0, attname, attributes[[attname]], prec = "integer")
    } else {
      ncatt_put(nc, 0, attname, attributes[[attname]])
    }
  }
  
  # Append counters
  nc$tt <- 0
  nc$nn <- 0
  nc$pp <- 0
  
  # Return connection
  nc
  
} # create_fronts


write_fronts <- function(nc, time, fronts) {
  
  # Total number of fronts
  n_fronts <- length(fronts)

  # Do we need to write front speeds?
  write_speed <- exists("frspeed", nc$var)
  
  # Write dimensions
  ncvar_put(nc, "time"   , time    , start = nc$tt + 1, count = 1)
  ncvar_put(nc, "nfronts", n_fronts, start = nc$tt + 1, count = 1) 
  
  if (n_fronts > 0) {
    
    # Unroll fronts
    n_points  <- numeric(n_fronts)
    longitude <- numeric()
    latitude  <- numeric()
    tfp       <- numeric()
    maggrad   <- numeric()
    if (write_speed) {
      frspeed <- numeric()
    }
    for (i in 1:n_fronts) {
      
      # Number of points in front
      n_points[i] <- length(fronts[[i]]$x)

      longitude <- c(longitude, fronts[[i]]$x      )
      latitude  <- c(latitude , fronts[[i]]$y      )
      tfp       <- c(tfp      , fronts[[i]]$tfp    )
      maggrad   <- c(maggrad  , fronts[[i]]$maggrad)
      if (write_speed) {
        frspeed <- c(frspeed, fronts[[i]]$frspeed)
      }
      
    } # i
    
    # Write dimensions
    ncvar_put(nc, "npoints", n_points, start = nc$nn + 1, count = n_fronts)
    
    # Write fronts
    t_points <- sum(n_points)
    ncvar_put(nc, "longitude", longitude, start = nc$pp + 1, count = t_points)
    ncvar_put(nc, "latitude" , latitude , start = nc$pp + 1, count = t_points)
    ncvar_put(nc, "tfp"      , tfp      , start = nc$pp + 1, count = t_points)
    ncvar_put(nc, "maggrad"  , maggrad  , start = nc$pp + 1, count = t_points)
    if (write_speed) {
      ncvar_put(nc, "frspeed", frspeed  , start = nc$pp + 1, count = t_points)
    }
    
  } # n_fronts > 0
  
  # Increment counters
  nc$tt <- nc$tt + 1
  nc$nn <- nc$nn + n_fronts
  nc$pp <- nc$pp + t_points
  
  # Return connection
  nc
  
} # write_fronts
