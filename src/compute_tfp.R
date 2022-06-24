# Load source
source("src/finite_diff.R")
source("src/smooth.R")

compute_tfp <- function(lon, lat, thetaw, u, v, opts) {
  
  # Constants
  dd <- pi / 180
  dy <- opts$radius * dd
  dx <- opts$radius * cos(dd * lat) * dd
  nx <- length(lon)
  ny <- length(lat)
  dlon <- (max(lon) - min(lon)) / (length(lon) - 1)
  dlat <- (max(lat) - min(lat)) / (length(lat) - 1)
  
  # Wrap domain
  if (opts$global) {
    if (opts$fourthorder) {
      guard_area <- opts$nsmooth + 4
    } else {
      guard_area <- opts$nsmooth + 2
    }
    mask1 <- (nx - guard_area + 1):nx
    mask3 <- 1:guard_area
    thetaw <- rbind(thetaw[mask1, ], thetaw, thetaw[mask3, ])
    if (opts$frspeed) {
      u <- rbind(u[mask1, ], u, u[mask3, ])
      v <- rbind(v[mask1, ], v, v[mask3, ])
    }
  }
  
  # Smoothing
  thetaw <- smooth_2d(thetaw, opts$nsmooth)
  if (opts$frspeed) {
    u <- smooth_2d(u, opts$nsmooth)
    v <- smooth_2d(v, opts$nsmooth)
  }
  
  
  # Compute gradients ---------------------------------------------------------
  
  # x gradient
  dtdx <- t(finite_diff(t(thetaw), dlon, opts$fourthorder))
  for (i in 1:ny) {
    dtdx[, i] <- dtdx[, i] / dx[i]
  } # i
  
  # y gradient
  dtdy <- finite_diff(thetaw, dlat, opts$fourthorder)
  dtdy <- dtdy / dy
  
  # Magnitude of gradient
  maggradt <- suppressWarnings(sqrt(dtdx * dtdx + dtdy * dtdy))
  maggradt[maggradt == 0] <- NA
  
  # x gradient of magnitude
  dmaggradtdx <- t(finite_diff(t(maggradt), dlon, opts$fourthorder))
  for (i in 1:ny) {
    dmaggradtdx[, i] <- dmaggradtdx[, i] / dx[i]
  } # i
  
  # y gradient of magnitude
  dmaggradtdy <- finite_diff(maggradt, dlat, opts$fourthorder)
  dmaggradtdy <- dmaggradtdy / dy
  
  # Thermal front parameter
  tfp <- (dtdx * dmaggradtdx + dtdy * dmaggradtdy) / maggradt
  
  
  # Laplacian of magnitude of gradient
  d2maggradtdx2 <- t(finite_diff2(t(maggradt), dlon, opts$fourthorder))
  for (i in 1:ny) {
    d2maggradtdx2[, i] <- d2maggradtdx2[, i] / dx[i]^2
  } # i
  
  d2maggradtdy2 <- finite_diff2(maggradt, dlat, opts$fourthorder)
  d2maggradtdy2 <- d2maggradtdy2 / dy^2
  
  # Front locator
  frloc <- d2maggradtdx2 + d2maggradtdy2
  
  # Magnitude of gradient of magnitude of gradient
  maggradmaggradt <- suppressWarnings(sqrt(dmaggradtdx * dmaggradtdx + 
                                             dmaggradtdy * dmaggradtdy))
  maggradmaggradt[maggradmaggradt == 0] <- NA
  
  # Magnitude of gradient in baroclinic zone
  delta <- 1 / sqrt(2)
  maggradt <- maggradt + t(delta * dx * t(maggradmaggradt))
  
  # Front speed
  if (opts$frspeed) {
    u_frsp  <- u * dmaggradtdx / maggradmaggradt
    v_frsp  <- v * dmaggradtdy / maggradmaggradt
    frspeed <- u_frsp + v_frsp
  } else {
    frspeed <- NULL
  } # frspeed
  
  # Shrink domain
  if (opts$global) {
    mask <- (guard_area + 1):(nx + guard_area)
    thetaw   <- thetaw  [mask, ]
    frloc    <- frloc   [mask, ]
    tfp      <- tfp     [mask, ]
    maggradt <- maggradt[mask, ]
    if (opts$frspeed)
      frspeed <- frspeed[mask, ]
  } # global
  
  # Return results
  list(
    thetaw  = thetaw, 
    frloc   = frloc, 
    tfp     = tfp, 
    maggrad = maggradt,
    frspeed = frspeed
  )
  
} # compute_tfp
