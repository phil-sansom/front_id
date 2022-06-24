# Load libraries
library(ncdf4)

# Determine dimensions
get_dimensions <- function(nc, varname, req_time = TRUE, tol = 0.001) {

  # Extract dimension short names and loop over dimensions
  for (dimname in names(nc$dim)) {
    
    # Check to see if dimension variable exists
    if (nc$dim[[dimname]]$create_dimvar) {
      
      # Search standard names
      buffer <- ncatt_get(nc, dimname, "standard_name")
      if (buffer$hasatt) {
        if (buffer$value == "longitude") {
          lon <- as.numeric(nc$dim[[dimname]]$vals)
          next
        } else if (buffer$value == "latitude") {
          lat <- as.numeric(nc$dim[[dimname]]$vals)
          next
        } else if (buffer$value == "time") {
          times      <- as.numeric(nc$dim[[dimname]]$vals)
          time_units <- nc$dim[[dimname]]$units
          calendar   <- nc$dim[[dimname]]$calendar
          next
        }
      } # hasatt
      
      # Search axes
      buffer <- ncatt_get(nc, dimname, "axis")
      if (buffer$hasatt) {
        if (buffer$value == "X") {
          lon <- as.numeric(nc$dim[[dimname]]$vals)
          next
        } else if (buffer$value == "Y") {
          lat <- as.numeric(nc$dim[[dimname]]$vals)
          next
        } else if (buffer$value == "T") {
          times      <- as.numeric(nc$dim[[dimname]]$vals)
          time_units <- nc$dim[[dimname]]$units
          calendar   <- nc$dim[[dimname]]$calendar
          next
        }
      } # hasatt
      
      # Search long names
      buffer <- ncatt_get(nc, dimname, "long_name")
      if (buffer$hasatt) {
        if (buffer$value == "longitude") {
          lon <- as.numeric(nc$dim[[dimname]]$vals)
          next
        } else if (buffer$value == "latitude") {
          lat <- as.numeric(nc$dim[[dimname]]$vals)
          next
        } else if (buffer$value == "time") {
          times      <- as.numeric(nc$dim[[dimname]]$vals)
          time_units <- nc$dim[[dimname]]$units
          calendar   <- nc$dim[[dimname]]$calendar
          next
        }
      } # hasatt
      
      # Search short names
      if (dimname %in% c("longitude", "lon")) {
        lon <- as.numeric(nc$dim[[dimname]]$vals)
        next
      } else if (dimname %in% c("latitude", "lat")) {
        lat <- as.numeric(nc$dim[[dimname]]$vals)
        next
      } else if (dimname == "time") {
        times      <- as.numeric(nc$dim[[dimname]]$vals)
        time_units <- nc$dim[[dimname]]$units
        calendar   <- nc$dim[[dimname]]$calendar
        next
      }

    } # create_dimvar
    
  } # i
  
  # Get units
  units <- ncatt_get(nc, varname, "units")
  if (units$hasatt) {
    units <- units$value
  } else {
    units <- ""
  }

  # Check dimensions
  if (! exists("lon")) {
    stop(paste("Unable to determine longitude dimension in", nc$filename))
  }
  if (! exists("lat")) {
    stop(paste("Unable to determine latitude dimension in", nc$filename))
  }
  if (! exists("times")) {
    if (req_time) {
      stop(paste("Unable to determine time dimension in", nc$filename))
    } else {
      times      <- NULL
      time_units <- NULL
      calendar   <- NULL
    }
  }
  
  # Check for floating point errors
  if (-90 - tol < min(lat) && min(lat) < -90) {
    lat[which.min(lat)] <- -90
  }
  if (+90 < max(lat) && max(lat) < +90 + tol) {
    lat[which.max(lat)] <- +90
  }

  # Return dimensions
  list(
    lon = lon, 
    lat = lat, 
    time = times, 
    time_units = time_units, 
    calendar = calendar, 
    units = units
  )
  
} # get_dimensions
