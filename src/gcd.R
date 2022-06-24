# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.8 (taken from Raster package)
# Licence GPL v3

# Vincenty formula for a sphere
# http://en.wikipedia.org/wiki/Great_circle_distance

gcd <- function(p1, p2, r = 6378137) {
  
  if (is.vector(p1)) {
    p1 <- matrix(p1, ncol = 2) 
  }
  if (missing(p2)) {
    p2 <- p1[       -1, , drop = FALSE]
    p1 <- p1[-nrow(p1), , drop = FALSE]
  } else if (is.vector(p2)) {
    p2 <- matrix(p2, ncol = 2) 
  }
  p1 <- p1 * pi / 180
  p2 <- p2 * pi / 180
  
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])	
  
  lon1 <- p[, 1]
  lat1 <- p[, 2]
  lon2 <- p[, 3]
  lat2 <- p[, 4]
  
  y <- sqrt((cos(lat2) * sin(lon1 - lon2))^2 + 
             (cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(lon1 - lon2))^2)
  x <- sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lon1 - lon2)
  
  dist <- r * atan2(y, x)
  
  as.vector(dist)
  
} # gcd
