create_diagnostics <- function(file, lon, lat, time_units, calendar, attributes, 
                               compression = 5, frspeed = TRUE) {
  
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
  thetaw_var  <- ncvar_def("thetaw", "K", list(lon_dim, lat_dim, time_dim),
                           missval = 9.9692099683868690e+36,
                           longname = "Wet-bulb potential temperature", 
                           prec = "float", 
                           compression = compression)
  frloc_var   <- ncvar_def("frloc", "K/m^3", list(lon_dim, lat_dim, time_dim),
                           missval = 9.9692099683868690e+36,
                           longname = "Front locating parameter", 
                           prec = "float", 
                           compression = compression)
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
  vars <- list(thetaw_var, frloc_var, tfp_var, maggrad_var)
  if (frspeed) {
    frspeed_var <- ncvar_def("frspeed", "m/s", list(lon_dim, lat_dim, time_dim),
                             missval = 9.9692099683868690e+36,
                             longname = "Front speed", 
                             prec = "float", 
                             compression = compression)
    vars <- c(vars, list(frspeed_var))
  } # frspeed
  
  # Create netCDF file
  nc <- nc_create(file, vars)
  
  # Description
  ncatt_put(nc, 0, "Conventions", "CF-1.9")
  ncatt_put(nc, 0, "title", "Diagnostic quantities for identificaiton of meteorological fronts.")
  ncatt_put(nc, 0, "source", "https://github.com/phil-sansom/front_id/tree/v0.9.2")
  ncatt_put(nc, 0, "references", "Hewson, T.D. (1998), Objective fronts. Met. Apps, 5: 37-65. https://doi.org/10.1017/S1350482798000553")
  
  # Standard names
  ncatt_put(nc, "longitude", "standard_name", "longitude")
  ncatt_put(nc, "latitude" , "standard_name", "latitude")
  ncatt_put(nc, "time"     , "standard_name", "time")
  ncatt_put(nc, "thetaw"   , "standard_name", "wet_bulb_potential_temperature")
  
  # Write global parameters
  attnames <- names(attributes)
  mask <- attnames %in% c("xfirst", "xinc", "xsize", "yfirst", "yinc", "ysize")
  for (attname in attnames) {
    if (is.logical(attributes[[attname]])) {
      ncatt_put(nc, 0, attname, attributes[[attname]], prec = "integer")
    } else {
      ncatt_put(nc, 0, attname, attributes[[attname]])
    }
  } # attname
  
  # Set counters
  nc$tt <- 0
  
  # Return connection
  nc
  
} # create_diagnostics
