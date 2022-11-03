# Load libraries --------------------------------------------------------------
library(ncdf4)
library(maps)


# Load source -----------------------------------------------------------------
source("src/maps_io.R")
source("src/fronts_io.R")


# Load maps using old parameters ----------------------------------------------
nc   <- open_maps("output/contour_then_mask_diagnostics.nc")
dims <- read_dimensions_maps(nc)
lon  <- dims$longitude
lat  <- dims$latitude
old_maps <- read_maps(nc, t = 1)
close_maps(nc)

# Extract fields
old_thetaw <- old_maps$thetaw
old_frloc  <- old_maps$frloc
old_tfp    <- old_maps$tfp


# Load fronts using old parameters --------------------------------------------
nc <- open_fronts("output/contour_then_mask.nc")
contour_then_mask <- read_fronts(nc, t = 1)
close_fronts(nc)


# Load maps using new parameters ----------------------------------------------
nc   <- open_maps("output/final_diagnostics.nc")
dims <- read_dimensions_maps(nc)
lon  <- dims$longitude
lat  <- dims$latitude
maps <- read_maps(nc, t = 1)
close_maps(nc)

# Extract fields
thetaw <- maps$thetaw
frloc  <- maps$frloc
tfp    <- maps$tfp


# Load maps using old parameters ----------------------------------------------
nc    <- open_fronts("output/final.nc")
final <- read_fronts(nc, t = 1)
close_fronts(nc)


# Front plotting function -----------------------------------------------------
front_line <- function(front) {
  
  n <- length(front$x)
  
  front$class <- integer(n)
  front$class[front$frspeed < -1.5] <- 1L
  front$class[-1.5 <= front$frspeed & front$frspeed <= 1.5] <- 2L
  front$class[front$frspeed > +1.5] <- 3L
  segments <- list()
  ss <- 0
  if (front$class[2] == front$class[1]) {
    x0 <- front$x[1]
    y0 <- front$y[1]
    s0 <- front$frspeed[1]
  } else {
    x0 <- 0.5 * (front$x[1] + front$x[2])
    y0 <- 0.5 * (front$y[1] + front$y[2])
    ss <- ss + 1
    segments[[ss]] <- list(
      x     = c(front$x[1], x0),
      y     = c(front$y[1], y0),
      class = front$class[1]
    )
  }
  alpha <- 2
  for (i in 2:(n - 1)) {
    
    if (front$class[i] != front$class[i + 1]) {
      x1 <- 0.5 * (front$x[i] + front$x[i + 1])
      y1 <- 0.5 * (front$y[i] + front$y[i + 1])
      ss <- ss + 1
      segments[[ss]] <- list(
        x     = c(x0, front$x[alpha:i], x1),
        y     = c(y0, front$y[alpha:i], y1),
        class = front$class[i]
      )
      alpha <- i + 1
      x0 <- x1
      y0 <- y1
    }
    
  } # i
  ss <- ss + 1
  segments[[ss]] <- list(
    x     = c(x0, front$x[alpha:n]),
    y     = c(y0, front$y[alpha:n]),
    class = front$class[n]
  )
  
  cols <- c("blue", "black", "red")
  for (i in 1:ss) {
    lines(segments[[i]], col = cols[segments[[i]]$class], lwd = 2)
  } # i
  
}


# Figures 3(b) and 3(c) -------------------------------------------------------

# Transparent red
myred <- rgb(1, 0, 0, 0.5)

# Set domain
lon_min  <- -45.0
lon_max  <- +45.0
lat_min  <- +30.0 
lat_max  <- +75.0
lon_mask <- lon_min - 1 <= lon & lon <= lon_max + 1
lat_mask <- lat_min - 1 <= lat & lat <= lat_max + 1

# Axis labels
lon_at <- seq(lon_min, lon_max, +15)
lat_at <- seq(lat_min, lat_max, +15)
lon_labels <- sprintf("%3.1f", lon_at)
lat_labels <- sprintf("%3.1f", lat_at)


# Open PDF device
pdf(
  file      = "fig/figure3bc.pdf",
  width     = 160 / 25.4, 
  height    = 43.25 / 25.4, 
  pointsize = 8
)

# Set layout
layout(matrix(1:2, 1, 2, byrow = TRUE))

# Set margins etc.
par(mar = c(1.5, 2, 1.0, 0.75), mgp = c(1, 0.5, 0), tcl = -1/3, ps = 8)

# Contour-then-mask
plot(
  x = 0, 
  y = 0, 
  type = "n", 
  asp  = 1, 
  xaxs = "i", 
  yaxs = "i",
  xlim = c(lon_min, lon_max), 
  ylim = c(lat_min, lat_max), 
  axes = FALSE, 
  ann  = FALSE,
  frame.plot = TRUE
)

# Add world map
map("world", 
    add    = TRUE, 
    fill   = TRUE, 
    col    = "lightgrey", 
    border = "darkgrey", 
    lwd   = 0.5)

# Add axes
axis(1, lon_at, lon_labels, cex.axis = 0.9, gap.axis = -1)
axis(2, lat_at, lat_labels, las = 1, cex.axis = 0.9)

# Add figure label
mtext("(b)", 3, line = 0, adj = 0)

# Add contours of wet bulb potential temperature
contour(
  x = lon[lon_mask], 
  y = lat[lat_mask], 
  z = old_thetaw[lon_mask, lat_mask],
  levels     = seq(floor(min(old_thetaw)), ceiling(max(old_thetaw)), 1),
  drawlabels = FALSE, 
  lwd        = 0.5, 
  col        = "black",
  add        = TRUE
)

# Add fronts
for (front in contour_then_mask) {
  
  # Check if front has any points inside domain
  in_x_domain <- any(lon_min <= front$x & front$x <= lon_max)
  in_y_domain <- any(lat_min <= front$y & front$y <= lat_max)
  
  # Add front if partially in domain
  if (in_x_domain & in_y_domain) 
    front_line(front)
  
}

# Final version
plot(
  x = 0, 
  y = 0,
  type = "n", 
  asp  = 1, 
  xaxs = "i", 
  yaxs = "i",
  xlim = c(lon_min, lon_max), 
  ylim = c(lat_min, lat_max), 
  axes = FALSE, 
  ann  = FALSE,
  frame.plot = TRUE
)

# Add world map
map(
  "world", 
  fill   = TRUE, 
  col    = "lightgrey", 
  border = "darkgrey", 
  lwd    = 0.5,
  add    = TRUE
)

# Add axes
axis(side = 1, at = lon_at, labels = lon_labels, cex.axis = 0.9, gap.axis = -1)
axis(side = 2, at = lat_at, labels = lat_labels, las = 1, cex.axis = 0.9)

# Add figure label
mtext("(c)", 3, line = 0, adj = 0)

# Add contours of wet bulb potential temperature
contour(
  x = lon[lon_mask], 
  y = lat[lat_mask], 
  z = thetaw[lon_mask, lat_mask],
  levels     = seq(floor(min(thetaw)), ceiling(max(thetaw)), 1),
  drawlabels = FALSE, 
  lwd        = 0.5, 
  col        = "black",
  add        = TRUE
)

# Add fronts
for (front in final) {
  
  # Check if front has any points inside domain
  in_x_domain <- any(lon_min <= front$x & front$x <= lon_max)
  in_y_domain <- any(lat_min <= front$y & front$y <= lat_max)
  
  # Add front if partially in domain
  if (in_x_domain & in_y_domain) 
    front_line(front)
  
}

# Close PDF device
dev.off()
