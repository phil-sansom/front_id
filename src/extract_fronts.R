# Load source
source("src/gcd.R")

extract_fronts <- function(contours, opts) {
  
  facets <- names(contours[[1]])
  facets <- facets[facets != "level"]
  
  fronts <- list()
  n <- 0
  
  # Loop over contours
  for (contour in contours) {
    
    # Find points the exceed thresholds
    mask <- which(contour$tfp < opts$thresh_t & 
                    opts$thresh_g < contour$maggrad)
    if (length(mask) > 0) {
      
      # First point
      alpha <- mask[1]
      append <- FALSE
      
      if (length(mask) > 1) {
        for (i in 1:(length(mask) - 1)) {
          # Check for a break in the contour
          if (mask[i] + 1 < mask[i + 1]) {
            # Store points up to break
            buffer <- alpha:mask[i]
            if (append) {
              # Append points to existing front
              for (facet in facets) {
                fronts[[n]][[facet]] <- 
                  c(fronts[[n]][[facet]],contour[[facet]][buffer])
              }
            } else {
              # ...otherwise increment front counter and 
              # store points up to break as new front
              n <- n + 1
              fronts[[n]] <- list()
              for (facet in facets) {
                fronts[[n]][[facet]] <- contour[[facet]][buffer]
              }
            } # append
            # Calculate distance between current point and ALL points in break
            gcdists <- gcd(c(contour$x[mask[i]],contour$y[mask[i]]),
                           cbind(contour$x[(mask[i] + 1):mask[i + 1]],
                                 contour$y[(mask[i] + 1):mask[i + 1]]),
                           r = opts$radius) / 1000
            if (all(gcdists <= opts$searchrad)) {
              # If all points in break are within search radius, then set the
              # append flag so that break is ignored and subsequent points are
              # appended to existing front
              append <- TRUE
              alpha <- mask[i] + 1
            } else {
              # Otherwise, unset append flag so that subsequent points are
              # stored as a new front
              append <- FALSE
              alpha <- mask[i + 1]
            } # gcdists <= opts$searchrad
          } # mask[i] + 1 < mask[i + 1]
          
        } # i
      } # length(mask) > 1
      
      # Deal with final point
      i <- length(mask)
      buffer <- alpha:mask[i]
      if (append) {
        # Append points to existing fronts...
        for (facet in facets) {
          fronts[[n]][[facet]] <- 
            c(fronts[[n]][[facet]], contour[[facet]][buffer])
        }
      } else {
        # ...otherwise store as new front
        n <- n + 1
        fronts[[n]] <- list()
        for (facet in facets) {
          fronts[[n]][[facet]] <- contour[[facet]][buffer]
        }
      } # append
      
    } # length(mask) > 0
    
  } # contour
  
  # Throw out short fronts
  i <- 1
  while (i <= length(fronts)) {
    
    # Compute total length of front
    npoints <- length(fronts[[i]]$x)
    if (npoints == 1) {
      gcdist <- 0
    } else {
      gcdist <- sum(gcd(cbind(fronts[[i]]$x, fronts[[i]]$y),
                        r = opts$radius)) / 1000
    } # npoints
    # If too short, remove it, else increment counter and check next front
    if (gcdist < opts$minlength) {
      fronts <- fronts[-i]
    } else {
      i <- i + 1
    } # minlength
    
  } # i
  
  # Return output
  fronts
  
} # extract_fronts
