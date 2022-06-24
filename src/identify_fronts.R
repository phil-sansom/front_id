# Load source
source("src/extract_fronts.R")

identify_fronts <- function(lon, lat, frloc, tfp, maggrad, frspeed, opts) {
  
  # Find zero contours of front locator
  if (opts$global) {
    
    # Triplicate domain to deal with lines crossing meridians
    contours <- contourLines(x = c(lon - 360, lon, lon + 360), 
                             y = lat, 
                             z = rbind(frloc, frloc, frloc), 
                             levels = 0)
    
    # Remove contours entirely in left most domain or at all in right most
    if (length(contours) > 0) {
      i <- 1
      while(i <= length(contours)) {
        
        if (all(contours[[i]]$x < -180) || 
            (any(contours[[i]]$x >= +180) && ! any(contours[[i]]$x < -180))) {
          contours <- contours[-i]
        } else if (any(contours[[i]]$x >= +180)) {
          mask <- -180 <= contours[[i]]$x & contours[[i]]$x < 180
          contours[[i]]$x <- contours[[i]]$x[mask]
          contours[[i]]$y <- contours[[i]]$y[mask]
          i <- i + 1
        } else {
          mask <- contours[[i]]$x < -180
          contours[[i]]$x[mask] <- contours[[i]]$x[mask] + 360
          i <- i + 1
        }
        
      } # i
    } # length(contours) > 0
    
    # Expand domain for interpolation
    nlon <- length(lon)
    lon <- c(lon[nlon] - 360, lon, lon[1] + 360)
    tfp <- rbind(tfp[nlon, ], tfp, tfp[1, ])
    maggrad <- rbind(maggrad[nlon, ], maggrad, maggrad[1, ])
    if (opts$frspeed)
      frspeed <- rbind(frspeed[nlon, ], frspeed, frspeed[1, ])
    
  } else {
    
    contours <- contourLines(lon, lat, frloc, levels = 0)
    
  } # global
  
  # Interpolate masking variables
  if (length(contours) > 0) {
    for (i in 1:length(contours)) {
      
      contours[[i]]$tfp <- 
        fields::interp.surface(list(x = lon, y = lat, z = tfp), 
                               cbind(contours[[i]]$x, contours[[i]]$y))
      contours[[i]]$maggrad <- 
        fields::interp.surface(list(x = lon, y = lat, z = maggrad), 
                               cbind(contours[[i]]$x, contours[[i]]$y))
      if (opts$frspeed) {
        contours[[i]]$frspeed <- 
          fields::interp.surface(list(x = lon, y = lat, z = frspeed), 
                                 cbind(contours[[i]]$x, contours[[i]]$y))
      }
      
    } # i
  } # length(contours > 0)
  
  # Extract fronts
  if (length(contours) > 0) {
    extract_fronts(contours, opts)
  } else {
    contours
  }
  
} # identify_fronts
