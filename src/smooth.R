smooth_2d <- function(x, n) {
  
  z <- x
  
  if (n > 0) {
    
    # Dimensions
    nx <- nrow(x)
    ny <- ncol(x)
    
    if (any(is.na(x))) {
      
      # Augment data at edges
      y <- matrix(NA, nx + 2, ny + 2)
 
      # Loop over smoothing cycles
      for (i in 1:n) {
        
        # Refresh data
        y[2:(nx + 1),2:(ny + 1)] <- z

        # Denominator
        na.y  <- is.na(y)
        nn <- matrix(5, nx, ny) -
          na.y[2:(nx + 1), 2:(ny + 1)] -
          na.y[1:nx      , 2:(ny + 1)] -
          na.y[3:(nx + 2), 2:(ny + 1)] -
          na.y[2:(nx + 1), 1:ny      ] -
          na.y[2:(nx + 1), 3:(ny + 2)]
        
        # Numerator
        yy <- y
        yy[na.y] <- 0
        z <- 
          yy[2:(nx + 1), 2:(ny + 1)] +
          yy[1:nx      , 2:(ny + 1)] +
          yy[3:(nx + 2), 2:(ny + 1)] +
          yy[2:(nx + 1), 1:ny      ] +
          yy[2:(nx + 1), 3:(ny + 2)]
        
        # Result
        z <- z / nn
        
      } # i
      
      z[is.na(x)] <- NA
      
    } else {

      # Loop over smoothing cycles
      for (i in 1:n) {

        y <- z

        # Body
        z[2:(nx - 1), 2:(ny - 1)] <- (y[2:(nx - 1), 2:(ny - 1)] +
                                      y[1:(nx - 2), 2:(ny - 1)] +
                                      y[3:nx      , 2:(ny - 1)] +
                                      y[2:(nx - 1), 1:(ny - 2)] +
                                      y[2:(nx - 1), 3:ny      ]) / 5
        
        # Corners
        z[ 1,  1] <- (y[ 1,  1] + y[     2,  1] + y[ 1,      2]) / 3
        z[nx,  1] <- (y[nx,  1] + y[nx - 1,  1] + y[nx,      2]) / 3
        z[ 1, ny] <- (y[ 1, ny] + y[     2, ny] + y[ 1, ny - 1]) / 3
        z[nx, ny] <- (y[nx, ny] + y[nx - 1, ny] + y[nx, ny - 1]) / 3

        # Edges
        z[2:(nx - 1), 1 ] <- (y[2:(nx - 1), 1] + y[1:(nx - 2), 1] +
                                y[3:nx, 1] + y[2:(nx - 1), 2]) / 4
        z[2:(nx - 1), ny] <- (y[2:(nx - 1), ny] + y[1:(nx - 2), ny] +
                                y[3:nx, ny] + y[2:(nx - 1), ny - 1]) / 4
        z[ 1, 2:(ny - 1)] <- (y[1, 2:(ny - 1)] + y[2, 2:(ny - 1)] +
                                y[1, 1:(ny - 2)] + y[1, 3:ny]) / 4
        z[nx, 2:(ny - 1)] <- (y[nx, 2:(ny - 1)] + y[nx - 1, 2:(ny - 1)] +
                                y[nx, 1:(ny - 2)] + y[nx, 3:ny]) / 4

      } # i

    } # any(is.na(x))
    
  } # n > 0
  
  # Return output  
  z
  
} # smooth_2d
