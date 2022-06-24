finite_diff <- function(x, h, fourthorder = FALSE) {
  
  ## Dimensions
  nx <- nrow(x)
  ny <- ncol(x)
  
  ## Initialize storage
  z  <- matrix(NA, nx, ny)
  hh <- matrix(NA, nx, ny)
  
  if (any(is.na(x))) {
    
    if (fourthorder) {
      
      ## Augment data at edges
      y <- matrix(NA, nx, ny + 8)
      y[, 5:(ny + 4)] <- x
      
      ## Find missing values
      not_na <- ! is.na(y)
      
      ## Find missing values
      plus4  <- not_na[, 9:(ny + 8)]
      plus3  <- not_na[, 8:(ny + 7)]
      plus2  <- not_na[, 7:(ny + 6)]
      plus1  <- not_na[, 6:(ny + 5)]
      zero   <- not_na[, 5:(ny + 4)]
      minus1 <- not_na[, 4:(ny + 3)]
      minus2 <- not_na[, 3:(ny + 2)]
      minus3 <- not_na[, 2:(ny + 1)]
      minus4 <- not_na[, 1: ny]
      
      ## Forward difference
      index <- which(! minus1 & zero & plus1 & plus2 & plus3 & plus4, 
                     arr.ind = TRUE)
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      index_plus2  <- index
      index_plus2[, 2] <- index[, 2] + 2
      index_plus3  <- index
      index_plus3[, 2] <- index[, 2] + 3
      index_plus4  <- index
      index_plus4[, 2] <- index[, 2] + 4
      z[index] <- -25 * x[index] + 48 * x[index_plus1] - 36 * x[index_plus2] + 
        16 * x[index_plus3] - 3 * x[index_plus4]
      hh[index] <- 12 * h
      
      ## Mostly forward difference
      index <- which(! minus2 & minus1 & zero & plus1 & plus2 & plus3, 
                     arr.ind = TRUE)
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      index_plus2 <- index
      index_plus2[, 2] <- index[, 2] + 2
      index_plus3 <- index
      index_plus3[, 2] <- index[, 2] + 3
      z[index] <- -3 * x[index_minus1] - 10 * x[index] + 18 * x[index_plus1] -
        6 * x[index_plus2] + x[index_plus3]
      hh[index] <- 12 * h
      
      ## Central difference
      index <- which(minus2 & minus1 & zero & plus1 & plus2, arr.ind = TRUE)
      index_minus2 <- index
      index_minus2[, 2] <- index[, 2] - 2
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      index_plus2 <- index
      index_plus2[, 2] <- index[, 2] + 2
      z[index] <- x[index_minus2] - 8 * x[index_minus1] + 8 * x[index_plus1] - 
        x[index_plus2]
      hh[index] <- 12 * h
      
      ## Mostly backward difference
      index <- which(minus3 & minus2 & minus1 & zero & plus1 & ! plus2, 
                     arr.ind = TRUE)
      index_minus3 <- index
      index_minus3[, 2] <- index[, 2] - 3
      index_minus2 <- index
      index_minus2[, 2] <- index[, 2] - 2
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      index_plus1 <- index
      index_plus1[, 2]  <- index[, 2] + 1
      z[index] <- - x[index_minus3] + 6 * x[index_minus2] - 
        18 * x[index_minus1] + 10 * x[index] + 3 * x[index_plus1]
      hh[index] <- 12 * h
      
      ## Backward difference
      index <- which(minus4 & minus3 & minus2 & minus1 & zero & ! plus1, 
                     arr.ind = TRUE)
      index_minus4 <- index
      index_minus4[, 2] <- index[, 2] - 4
      index_minus3 <- index
      index_minus3[, 2] <- index[, 2] - 3
      index_minus2 <- index
      index_minus2[, 2] <- index[, 2] - 2
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      z[index] <- 3 * x[index_minus4] - 16 * x[index_minus3] + 
        36 * x[index_minus2] - 48 * x[index_minus1] + 25 * x[index]
      hh[index] <- 12 * h
      
      ## Forward difference
      index <- which(! minus1 & zero & plus1 & plus2 & plus3 & ! plus4, 
                     arr.ind = TRUE)
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      index_plus2 <- index
      index_plus2[, 2] <- index[, 2] + 2
      index_plus3 <- index
      index_plus3[, 2] <- index[, 2] + 3
      z[index] <- -11 * x[index] + 18 * x[index_plus1] - 9 * x[index_plus2] + 
        2 * x[index_plus3]
      hh[index] <- 6 * h
      
      ## Mostly forward difference
      index <- which(! minus2 & minus1 & zero & plus1 & plus2 & ! plus3, 
                     arr.ind = TRUE)
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      index_plus2 <- index
      index_plus2[, 2] <- index[, 2] + 2
      z[index] <- -2 * x[index_minus1] - 3 * x[index] + 6 * x[index_plus1] - 
        x[index_plus2]
      hh[index] <- 6 * h
      
      ## Mostly backward difference
      index <- which(! minus3 & minus2 & minus1 & zero & plus1 & ! plus2, 
                     arr.ind = TRUE)
      index_minus2 <- index
      index_minus2[, 2] <- index[, 2] - 2
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      z[index] <- x[index_minus2] - 6 * x[index_minus1] + 3 * x[index] + 
        2 * x[index_plus1]
      hh[index] <- 6 * h
      
      ## Backward difference
      index <- which(! minus4 & minus3 & minus2 & minus1 & zero & ! plus1, 
                     arr.ind = TRUE)
      index_minus3 <- index
      index_minus3[, 2] <- index[, 2] - 3
      index_minus2 <- index
      index_minus2[, 2] <- index[, 2] - 2
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      z[index] <- -2 * x[index_minus3] + 9 * x[index_minus2] - 
        18 * x[index_minus1] + 11 * x[index]
      hh[index] <- 6 * h
      
      ## Forward difference
      index <- which(! minus1 & zero & plus1 & plus2 & ! plus3, arr.ind = TRUE)
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      index_plus2 <- index
      index_plus2[, 2] <- index[, 2] + 2
      z[index] <- -3 * x[index] + 4 * x[index_plus1] - x[index_plus2]
      hh[index] <- 2 * h
      
      ## Central difference
      index <- which(! minus2 & minus1 & zero & plus1 & ! plus2, 
                     arr.ind = TRUE)
      index_minus1 <- index
      index_minus1[, 2] <- index_minus1[, 2] - 1
      index_plus1 <- index
      index_plus1[, 2] <- index_plus1[, 2] + 1
      z[index] <- x[index_plus1] - x[index_minus1]
      hh[index] <- 2 * h
      
      ## Backward difference
      index <- which(! minus3 & minus2 & minus1 & zero & ! plus1, 
                     arr.ind = TRUE)
      index_minus2 <- index
      index_minus2[, 2] <- index[, 2] - 2
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      z[index] <- x[index_minus2] - 4 * x[index_minus1] + 3 * x[index]
      hh[index] <- 2 * h
      
      ## Simple forward difference
      index <- which(! minus1 & zero & plus1 & ! plus2, arr.ind = TRUE)
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      z[index] <- x[index_plus1] - x[index]
      hh[index] <- h
      
      ## Simple backward difference
      index <- which(! minus2 & minus1 & zero & ! plus1, arr.ind = TRUE)
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      z[index] <- x[index] - x[index_minus1]
      hh[index] <- h
      
    } else {
      
      ## Augment data at edges
      y <- matrix(NA, nx, ny + 2)
      y[, 2:(ny + 1)] <- x
      
      ## Find missing values
      not_na <- ! is.na(y)
      
      ## Find missing values
      plus1  <- not_na[, 3:(ny + 2)]
      zero   <- not_na[, 2:(ny + 1)]
      minus1 <- not_na[, 1:ny]
      
      ## Forward difference
      index <- which(! minus1 & zero & plus1, arr.ind = TRUE)
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      z[index] <- x[index_plus1] - x[index]
      hh[index] <- h
      
      ## Central difference
      index <- which(minus1 & zero & plus1, arr.ind = TRUE)
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      z[index] <- x[index_plus1] - x[index_minus1]
      hh[index] <- 2 * h
      
      ## Backward difference
      index <- which(minus1 & zero & ! plus1, arr.ind = TRUE)
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      z[index] <- x[index] - x[index_minus1]
      hh[index] <- h
      
    } ## fourthorder
    
  } else {
    
    if (fourthorder) {
      
      hh <- 12 * h
      
      z[, 1] <- -25 * x[, 1] + 48 * x[, 2] - 36 * x[, 3] + 16 * x[, 4] - 3 * x[, 5]
      z[, 2] <- -3 * x[, 1] - 10 * x[,2] + 18 * x[, 3] - 6 * x[, 4] + x[, 5]
      z[, 3:(ny - 2)] <- x[, 1:(ny - 4)] - 8 * x[, 2:(ny - 3)] + 8 * x[, 4:(ny - 1)] - x[, 5:ny]
      z[, ny - 1] <- - x[,ny - 4] +  6 * x[, ny - 3] - 18 * x[, ny - 2] + 10 * x[, ny - 1] + 3 * x[, ny]
      z[, ny] <- 3 * x[,ny - 4] - 16 * x[, ny - 3] + 36 * x[, ny - 2] - 48 * x[, ny - 1] + 25 * x[, ny]
      
    } else {
      
      z [, 1] <- x[, 2] - x[, 1]
      hh[, 1] <- h
      z [, 2:(ny - 1)] <- x[, 3:ny] -  x[, 1:(ny - 2)]
      hh[, 2:(ny - 1)] <- 2 * h
      z [, ny] <- x[, ny] - x[, (ny - 1)]
      hh[, ny] <- h
      
    } ## fourthorder
    
  } ## any(is.na(x))
  
  z / hh
  
} ## finite_diff


finite_diff2 <- function(x, h, fourthorder = FALSE) {
  
  ## Dimensions
  nx <- nrow(x)
  ny <- ncol(x)
  
  ## Initialize storage
  z  <- matrix(NA, nx, ny)
  hh <- matrix(NA, nx, ny)
  
  if (any(is.na(x))) {
    
    if (fourthorder) {
      
      ## Augment data at edges
      y <- matrix(NA, nx, ny + 8)
      y[, 5:(ny + 4)] <- x
      
      ## Find missing values
      not_na <- ! is.na(y)
      
      ## Find missing values
      plus4  <- not_na[, 9:(ny + 8)]
      plus3  <- not_na[, 8:(ny + 7)]
      plus2  <- not_na[, 7:(ny + 6)]
      plus1  <- not_na[, 6:(ny + 5)]
      zero   <- not_na[, 5:(ny + 4)]
      minus1 <- not_na[, 4:(ny + 3)]
      minus2 <- not_na[, 3:(ny + 2)]
      minus3 <- not_na[, 2:(ny + 1)]
      minus4 <- not_na[, 1: ny]
      
      ## Forward difference
      index <- which(! minus1 & zero & plus1 & plus2 & plus3 & plus4, 
                     arr.ind = TRUE)
      index_plus1  <- index
      index_plus1[, 2] <- index[, 2] + 1
      index_plus2 <- index
      index_plus2[, 2] <- index[, 2] + 2
      index_plus3 <- index
      index_plus3[, 2] <- index[, 2] + 3
      index_plus4 <- index
      index_plus4[, 2] <- index[, 2] + 4
      z[index] <- 35 * x[index] - 104 * x[index_plus1] - 114 * x[index_plus2] - 
        56 * x[index_plus3] + 11 * x[index_plus4]
      hh[index] <- 12 * h * h
      
      ## Mostly forward difference
      index <- which(! minus2 & minus1 & zero & plus1 & plus2 & plus3, 
                     arr.ind = TRUE)
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      index_plus2 <- index
      index_plus2[, 2] <- index[, 2] + 2
      index_plus3 <- index
      index_plus3[, 2] <- index[, 2] + 3
      z[index] <- 11 * x[index_minus1] - 20 * x[index] + 6 * x[index_plus1] + 
        4 * x[index_plus2] - x[index_plus3]
      hh[index] <- 12 * h * h
      
      ## Central difference
      index <- which(minus2 & minus1 & zero & plus1 & plus2, arr.ind = TRUE)
      index_minus2 <- index
      index_minus2[, 2] <- index[, 2] - 2
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      index_plus2 <- index
      index_plus2[, 2] <- index[, 2] + 2
      z[index] <- - x[index_minus2] + 16 * x[index_minus1] - 30 * x[index] + 
        16 * x[index_plus1] - x[index_plus2]
      hh[index] <- 12 * h * h
      
      ## Mostly backward difference
      index <- which(minus3 & minus2 & minus1 & zero & plus1 & ! plus2, 
                     arr.ind = TRUE)
      index_minus3 <- index
      index_minus3[, 2] <- index[, 2] - 3
      index_minus2 <- index
      index_minus2[, 2] <- index[, 2] - 2
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      z[index] <- - x[index_minus3] + 4 * x[index_minus2] + 
        6 * x[index_minus1] - 20 * x[index] + 11 * x[index_plus1]
      hh[index] <- 12 * h * h
      
      ## Backward difference
      index <- which(minus4 & minus3 & minus2 & minus1 & zero & ! plus1, 
                     arr.ind = TRUE)
      index_minus4 <- index
      index_minus4[, 2] <- index[, 2] - 4
      index_minus3 <- index
      index_minus3[, 2] <- index[, 2] - 3
      index_minus2 <- index
      index_minus2[, 2] <- index[, 2] - 2
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      z[index] <- 11 * x[index_minus4] - 56 * x[index_minus3] + 
        114 * x[index_minus2] - 104 * x[index_minus1] + 35 * x[index]
      hh[index] <- 12 * h * h
      
      ## Forward difference
      index <- which(! minus1 & zero & plus1 & plus2 & plus3 & ! plus4, 
                     arr.ind = TRUE)
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      index_plus2 <- index
      index_plus2[, 2] <- index[, 2] + 2
      index_plus3 <- index
      index_plus3[, 2] <- index[, 2] + 3
      z[index] <- 2 * x[index] - 5 * x[index_plus1] + 4 * x[index_plus2] - 
        x[index_plus3]
      hh[index] <- h * h
      
      ## Mostly forward difference
      index <- which(! minus2 & minus1 & zero & plus1 & plus2 & ! plus3, 
                     arr.ind = TRUE)
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      index_plus2 <- index
      index_plus2[, 2] <- index[, 2] + 2
      z[index] <- x[index_minus1] - 2 * x[index] + x[index_plus1]
      hh[index] <- h * h
      
      ## Mostly backward difference
      index <- which(! minus3 & minus2 & minus1 & zero & plus1 & ! plus2, 
                     arr.ind = TRUE)
      index_minus2 <- index
      index_minus2[, 2] <- index[, 2] - 2
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      z[index] <- x[index_minus1] - 2 * x[index] + x[index_plus1]
      hh[index] <- h * h
      
      ## Backward difference
      index <- which(! minus4 & minus3 & minus2 & minus1 & zero & ! plus1, 
                     arr.ind = TRUE)
      index_minus3 <- index
      index_minus3[, 2] <- index[, 2] - 3
      index_minus2 <- index
      index_minus2[, 2] <- index[, 2] - 2
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      z[index] <- - x[index_minus3] + 4 * x[index_minus2] - 
        5 * x[index_minus1] + 2 * x[index]
      hh[index] <- h * h
      
      ## Forward difference
      index <- which(! minus1 & zero & plus1 & plus2 & ! plus3, 
                     arr.ind = TRUE)
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      index_plus2 <- index
      index_plus2[, 2] <- index[, 2] + 2
      z[index] <- x[index] - 2 * x[index_plus1] + x[index_plus2]
      hh[index] <- h * h
      
      ## Central difference
      index <- which(! minus2 & minus1 & zero & plus1 & ! plus2, 
                     arr.ind = TRUE)
      index_minus1 <- index
      index_minus1[, 2] <- index_minus1[, 2] - 1
      index_plus1 <- index
      index_plus1[, 2] <- index_plus1[, 2] + 1
      z[index] <- x[index_plus1] - 2 * x[index] + x[index_minus1]
      hh[index] <- h * h
      
      ## Backward difference
      index <- which(! minus3 & minus2 & minus1 & zero & ! plus1, 
                     arr.ind = TRUE)
      index_minus2 <- index
      index_minus2[, 2] <- index[, 2] - 2
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      z[index] <- x[index_minus2] - 2 * x[index_minus1] + x[index]
      hh[index] <- h * h
      
    } else {
      
      ## Augment data at edges
      y <- matrix(NA, nx, ny + 4)
      y[, 3:(ny + 2)] <- x
      
      ## Find missing values
      not_na <- ! is.na(y)
      
      ## Find missing values
      plus2  <- not_na[, 5:(ny + 4)]
      plus1  <- not_na[, 4:(ny + 3)]
      zero   <- not_na[, 3:(ny + 2)]
      minus1 <- not_na[, 2:(ny + 1)]
      minus2 <- not_na[, 1: ny]
      
      ## Forward difference
      index <- which(! minus1 & zero & plus1 & plus2, arr.ind = TRUE)
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      index_plus2 <- index
      index_plus2[, 2] <- index[, 2] + 2
      z[index] <- x[index] - 2 * x[index_plus1] + x[index_plus2]
      hh[index] <- h * h
      
      ## Central difference
      index <- which(plus1 & zero & minus1, arr.ind = TRUE)
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      index_plus1 <- index
      index_plus1[, 2] <- index[, 2] + 1
      z[index] <- x[index_minus1] - 2 * x[index] + x[index_plus1]
      hh[index] <- h * h
      
      ## Backward difference
      index <- which(minus2 & minus1 & zero & ! plus1, arr.ind = TRUE)
      index_minus2 <- index
      index_minus2[, 2] <- index[, 2] - 2
      index_minus1 <- index
      index_minus1[, 2] <- index[, 2] - 1
      z[index] <- x[index_minus2] - 2 * x[index_minus1] + 1 * x[index]
      hh[index] <- h * h
      
    } ## fourthorder
    
  } else {
    
    if (fourthorder) {
      
      hh <- 12 * h * h
      
      z <- x
      z[, 1] <- 35 * x[, 1] - 104 * x[, 2] + 114 * x[, 3] - 56 * x[, 4]+ 11 * x[, 5]
      z[, 2] <- 11 * x[, 1] -  20 * x[, 2] + 6 * x[, 3] + 4 * x[, 4] - 1 * x[, 5]
      z[, 3:(ny - 2)] <- -x[, 1:(ny - 4)] + 16 * x[, 2:(ny - 3)] - 30 * x[, 3:(ny - 2)] + 16 * x[, 4:(ny-1)] - x[, 5:ny]
      z[, ny - 1] <- - 1 * x[, ny - 4] + 4 * x[, ny - 3] + 6 * x[,ny - 2] - 20 * x[, ny - 1] + 11 * x[, ny]
      z[, ny] <- 11 * x[, ny - 4] - 56 * x[, ny - 3] + 114 * x[, ny - 2] - 104 * x[, ny - 1] + 35 * x[, ny]
      
    } else {
      
      hh <- h * h
      
      z[, 1]          <- x[, 1]          - 2 * x[, 2]          + x[, 3]
      z[, 2:(ny - 1)] <- x[, 1:(ny - 2)] - 2 * x[, 2:(ny - 1)] + x[, 3:ny]
      z[, ny]         <- x[, ny - 2]     - 2 * x[, ny - 1]     + x[, ny]
      
    } ## fourthorder
    
  }
  
  z / hh
  
} ## finite_diff2
