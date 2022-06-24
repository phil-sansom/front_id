lonflip = function(x, lon) {
  
  if (180 < lon[1]) {
    lon <- lon - 360
  } else if (lon[1] < -180) {
    lon <- lon + 360
  }
  
  mask1 <- 180 <= lon
  mask2 <- ! mask1
  y <- rbind(x[mask1, ], x[mask2, ])
  z <- c(lon[mask1] - 360, lon[mask2])
  
  list(x = y, lon = z)
  
} # lonflip
