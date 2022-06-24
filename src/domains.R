parse_domain <- function(domain_string) {
  
  # Parse domain string
  parsed <- suppressWarnings(as.numeric(strsplit(domain_string, ",")[[1]]))
  
  # Check domain string
  if (any(is.na(parsed))) {
    stop("Domain format not recognised (lon1,lon2,lat1,lat2)")
  }
  if (length(parsed) < 4) {
    stop("Domain should have four components (lon1,lon2,lat1,lat2)")
  }
  
  # Extract domain
  domain <- list(
    minlon = parsed[1],
    maxlon = parsed[2],
    minlat = parsed[3],
    maxlat = parsed[4]
  )

} # parse_domain


check_domain = function(lon, lat, domain, exclude_poles = FALSE, tol = 0.001) {
  
  # Spacing
  dlon <- (max(lon) - min(lon)) / (length(lon) - 1)
  dlat <- (max(lat) - min(lat)) / (length(lat) - 1)
  
  # If latitudes are the wrong way round, then stop
  if (domain$maxlat < domain$minlat) {
    stop("maxlat < minlat")
  }
  
  # If longitudes are the wrong way round, then stop
  if (domain$maxlon < domain$minlon) {
    stop("maxlon < minlon")
  }
  
  # If total longitude too great, then stop
  if (domain$maxlon - domain$minlon > 360 + tol) {
    stop("maxlon - minlon > 360")
  }
  
  # Check latitude bounds
  if (domain$minlat < -90 - tol) {
    stop("minlat < -90")
  }
  if (+90 - tol <= domain$minlat) {
    stop("minlat >= +90")
  }
  if (domain$maxlat <= -90 + tol) {
    stop("maxlat <= -90")
  }
  if (+90 + tol < domain$maxlat) {
    stop("maxlat > +90")
  }
  
  # Check longitude bounds
  if (domain$minlon < -360 - tol) {
    stop("minlon < -360")
  }
  if (+360 - tol <= domain$minlon) {
    stop("minlon >= +360")
  }
  if (domain$maxlon <= -360 + tol) {
    stop("maxlon <= -360")
  }
  if (+360 + tol < domain$maxlon) {
    stop("maxlon > +360")
  }
  
  # Transform bounds if needed
  if (360 - dlon < domain$maxlon - domain$minlon ||
      abs((360 - dlon) - (domain$maxlon - domain$minlon)) < tol) {
    domain$minlon <- -180
    domain$maxlon <- +180
  } else if (domain$minlon < -180  - tol && domain$maxlon < 0 + tol) {
    domain$minlon <- domain$minlon + 360
    domain$maxlon <- domain$maxlon + 360
  } else if (+180  + tol < domain$minlon) {
    domain$minlon <- domain$minlon - 360
    domain$maxlon <- domain$maxlon - 360
  }
  
  # Exclude poles, if requested
  if (exclude_poles) {
    if (abs(-90 - domain$minlat) < tol)
      domain$minlat <- min(lat[lat > -90 + tol])
    if (abs(+90 - domain$maxlat) < tol)
      domain$maxlat <- max(lat[lat < +90 - tol])
  }
  
  # Return domain
  domain

} # check_domain


compare_domains <- function(lon, lat, domain, tol = 0.001) {
  
  # Grid spacing
  dlon <- (max(lon) - min(lon)) / (length(lon) - 1)
  dlat <- (max(lat) - min(lat)) / (length(lat) - 1)
  
  # Grid extent
  minlon <- min(lon)
  maxlon <- max(lon)
  minlat <- min(lat)
  maxlat <- max(lat)
  
  # Check latitudinal extent
  if (domain$minlat < minlat - dlat / 2 - tol || 
      maxlat + dlat / 2 + tol < domain$maxlat) {
    stop("Requested domain exceeds the latitudinal extent of the data")
  }
  
  # Check longitudinal extent
  if (abs((360 - dlon) - (maxlon - minlon)) < tol) {
    
    # If data have global extent, then any domain is fine
    flag <- FALSE
    
  } else if ((domain$minlon       < minlon - dlon / 2 - tol || maxlon + dlon / 2 + tol < domain$maxlon      ) &&
             (domain$minlon - 360 < minlon - dlon / 2 - tol || maxlon + dlon / 2 + tol < domain$maxlon - 360) &&
             (domain$minlon + 360 < minlon - dlon / 2 - tol || maxlon + dlon / 2 + tol < domain$maxlon + 360)) {
    
    # Otherwise flag if domain exceeds data extent by more than a half grid
    flag <- TRUE
    
  } else {
    
    flag <- FALSE
    
  }
  if (flag) {
    stop("Requested domain exceeds the longitudinal extent of the data")
  }
  
  # Return domain
  domain
  
}  # compare_domain


set_domain <- function(lon, lat) {
  
  # Set domain equal to extent of data
  domain <- list(
    minlon = min(lon),
    maxlon = max(lon),
    minlat = min(lat),
    maxlat = max(lat)
  )
  
} # set_domain
