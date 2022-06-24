# Load libraries
library(ncdf4)

open_maps <- function(file, write = FALSE) {
  
  nc <- nc_open(file, write)
  nc$tt <- nc$dim$time$len
  
  nc
  
} # open_maps


read_attributes_maps <- function(nc) {
  
  ncatt_get(nc, 0)
  
} # read_attributes_maps


read_dimensions_maps  <- function(nc) {
  
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
  
} # read_dimensions_maps


get_variables_maps <- function(nc) {
  
  names(nc$var)
  
} # get_variables_maps


read_maps <- function(nc, t) {
  
  fields <- list()
  
  varids <- names(nc$var)
  for (varid in varids) {
    fields[[varid]] <- 
      ncvar_get(nc, varid, start = c(1, 1, t), count = c(-1, -1, 1))
  }
  
  # Return fields
  fields
  
} # read_maps


close_maps <- function(nc) {
  
  nc_close(nc)
  
} # close_maps


create_maps <- function(file, lon, lat, time_units, calendar, attributes, 
                        fields = TRUE, 
                        classified = FALSE, 
                        compression = 5,
                        title = "Meteorological fronts", 
                        history = NULL, 
                        frspeed = TRUE) {
  
  # Define dimensions
  lon_dim  <- ncdim_def("longitude", "degrees_east" , lon, 
                        longname = "Longitude")
  lat_dim  <- ncdim_def("latitude" , "degrees_north", lat, 
                        longname = "Latitude")
  time_dim <- ncdim_def("time", time_units, numeric(), 
                        unlim = TRUE, 
                        calendar = calendar, 
                        longname = "Time")
  
  # Define variables
  if (classified) {
    
    cold_var <- ncvar_def("cold_fronts", "id", list(lon_dim, lat_dim, time_dim),
                          missval = -32767, 
                          longname = "Cold fronts", 
                          prec = "short", 
                          compression = compression)
    warm_var <- ncvar_def("warm_fronts", "id", list(lon_dim, lat_dim, time_dim),
                          missval = -32767, 
                          longname = "Warm fronts", 
                          prec = "short", 
                          compression = compression)
    stat_var <- ncvar_def("stat_fronts", "id", list(lon_dim, lat_dim, time_dim),
                          missval = -32767, 
                          longname = "Stationary fronts", 
                          prec = "short", 
                          compression = compression)
    vars <- list(cold_var, warm_var, stat_var)
    field_names <- c("cold_fronts", "warm_fronts", "stat_fronts")
    
  } else {
    
    fronts_var <- ncvar_def("fronts", "id", list(lon_dim, lat_dim, time_dim),
                            missval = -32767, 
                            longname = "Fronts", 
                            prec = "short", 
                            compression = compression)
    vars <- list(fronts_var)
    field_names <- "fronts"
    
  }
  if (fields) {
    
    tfp_var     <- ncvar_def("tfp", "K/m^2", list(lon_dim, lat_dim, time_dim),
                             missval = 9.9692099683868690e+36, 
                             longname = "Thermal front parameter", 
                             prec = "float", 
                             compression = compression)
    maggrad_var <- ncvar_def("maggrad", "K/m", list(lon_dim, lat_dim, time_dim),
                             missval = 9.9692099683868690e+36, 
                             longname = "Magnitude of gradient", 
                             prec = "float", 
                             compression = compression)
    field_vars <- list(tfp_var, maggrad_var)
    vars <- c(vars, field_vars)
    field_names <- c(field_names, "tfp", "maggrad")
    
  }
  if (frspeed) {
    
    frspeed_var <- ncvar_def("frspeed", "m/s", list(lon_dim, lat_dim, time_dim),
                             missval = 9.9692099683868690e+36, 
                             longname = "Front speed", 
                             prec = "float", 
                             compression = compression)
    vars <- c(vars,list(frspeed_var))
    field_names <- c(field_names,"frspeed")
    
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
  
  # Write global parameters
  mask <- attnames %in% c("Conventions", "title", "source", "references", "history",
                          "xfirst", "xinc", "xsize", "xgrid",
                          "yfirst", "yinc", "ysize", "ygrid")
  attnames <- attnames[! mask]
  for (attname in attnames) {
    if (is.logical(attributes[[attname]])) {
      ncatt_put(nc, 0, attname, attributes[[attname]], prec = "integer")
    } else {
      ncatt_put(nc, 0, attname, attributes[[attname]])
    }
  }
  
  # Append counter
  nc$tt <- 0
  
  # Return connection
  nc
  
} # create_maps


write_maps <- function(nc, time, x) {
  
  # Get fields to be written
  fields <- names(x)
  
  # Write fields
  ncvar_put(nc, "time", time, start = nc$tt + 1, count = 1)
  for (field in fields)
    if (! is.null(x[[field]])) {
      ncvar_put(nc, field, x[[field]], 
                start = c(1, 1, nc$tt + 1), count = c(-1, -1, 1))
    }
  
  # Increment counter
  nc$tt <- nc$tt + 1
  
  # Return connection
  nc
  
} # write_maps
