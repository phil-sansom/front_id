# Compare dimensions of two variables
compare_dimensions <- function(nc1, varname1, nc2, varname2, tol = 0.001) {
  
  # Get dimensions
  dims1 <- get_dimensions(nc1, varname1, tol = tol)
  dims2 <- get_dimensions(nc2, varname2, tol = tol)
  
  # Check longitude dimension
  if (any(tol < abs(dims1$lon - dims2$lon))) {
    stop(paste("Longitude dimension doesn't match between",
               varname1, "in", nc1$filename, "and",
               varname2, "in", nc2$filename))
  }
  
  # Check latitude dimension
  if (any(tol < abs(dims1$lat - dims2$lat))) {
    stop(paste("Latitude dimension doesn't match between",
               varname1, "in", nc1$filename, "and", 
               varname2, "in", nc2$filename))
  }
  
  # Check time dimension
  if (any(tol < abs(dims1$time - dims2$time))) {
    stop(paste("Time dimension doesn't match between",
               varname1, "in", nc1$filename, "and",
               varname2, "in", nc2$filename))
  }
  
  # Check vertical dimension
  if (exists("z", dims1) && !exists("z", dims2)) {
    stop(paste(varname1, "in", nc1$filename, "has a vertical dimension, but",
               varname2, "in", nc2$filename, "does not"))
  }
  if (exists("z", dims2) && !exists("z", dims1)) {
    stop(paste(varname2, "in", nc2$filename, "has a vertical dimension, but",
               varname1, "in", nc1$filename, "does not"))
  }
  if (exists("z", dims1) && exists("z", dims2)) {
    if (any(tol < abs(dimst$z - dims$z)))
      stop(paste("Vertical dimension doesn't match between",
                 varname1, "in", nc1$filename, "and",
                 varname2, "in", nc2$filename))
  }
  
} # compare_dimensions
