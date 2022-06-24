make_maps <- function(x, lon, lat, sigma = 0, speed = FALSE) {
  
  # Extract dimensions
  n_lon <- length(lon)
  n_lat <- length(lat)
  lonr <- pi * lon / 180

  # Initialize storage
  fronts  <- array(NA, c(n_lon, n_lat))
  tfp     <- array(NA, c(n_lon, n_lat))
  maggrad <- array(NA, c(n_lon, n_lat))
  if (speed) {
    frspeed <- array(NA, c(n_lon, n_lat))
  }
  
  # Number of fronts
  n_fronts <- length(x)
  
  if (n_fronts > 0) {
    
    # Loop over fronts
    for (i in 1:n_fronts) {
      
      # Extract current front
      y <- x[[i]]
      
      # Jitter lat and lon
      if (sigma > 0) {
        xy <- rnorm(2) * sigma
      }
      else {
        xy <- c(0, 0)
      }
      
      # Number of points
      n_points <- length(y$x)
      
      # Loop over points
      for (j in 1:n_points) {
        
        # Jitter
        lonj <- y$x[j] + xy[2]
        latj <- y$y[j] + xy[1]
        
        # Check for crossing poles
        if (latj > +90) {
          latj <- 180 - latj
          lonj <- lonj + 180
        } else if (latj < -90) {
          latj <- -180 - latj
          lonj <- lonj + 180
        }
        if (lonj > 180) {
          lonj <- lonj - 360
        }
        lonpr <- pi * lonj / 180
        
        # Find nearest grid point to point location
        latp <- which.min(abs(lat - latj))
        lonp <- which.min(acos(cos(abs(lonr - lonpr))))
        
        # Assign data to new grid point
        fronts [lonp, latp] <- i
        tfp    [lonp, latp] <- y$tfp[j]
        maggrad[lonp, latp] <- y$maggrad[j]
        if (speed) {
          frspeed[lonp, latp] <- y$frspeed[j]
        }
        
      } # j
      
    } # i
    
  } # n_fronts > 0
  
  # Return output
  if (speed) {
    list(fronts = fronts, tfp = tfp, maggrad = maggrad, frspeed = frspeed)
  } else {
    list(fronts = fronts, tfp = tfp, maggrad = maggrad)
  }

}
