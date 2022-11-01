# Load libraries --------------------------------------------------------------
library(ncdf4)
library(maps)


# Load source -----------------------------------------------------------------
source("src/maps_io.R")
source("src/fronts_io.R")
source("src/classify_fronts.R")


# Load maps -------------------------------------------------------------------
nc   <- open_maps("output/contour_then_mask_diagnostics.nc")
dims <- read_dimensions_maps(nc)
lon  <- dims$longitude
lat  <- dims$latitude
maps <- read_maps(nc, t = 1)
close_maps(nc)

# Extract fields
thetaw <- maps$thetaw
frloc  <- maps$frloc
tfp    <- maps$tfp

# TFP mask
tfp_mask <- (tfp < -5e-11) * 1
tfp_mask[tfp_mask == 0] <- NA


# Load fronts -----------------------------------------------------------------
nc <- open_fronts("output/contour_then_mask.nc")
contour_then_mask <- read_fronts(nc, t = 1)
close_fronts(nc)

# Classify fronts
contour_then.mask_classified <- classify_fronts(contour_then_mask, 
                                                threshold = 1.5)


# Figure 1(b) -----------------------------------------------------------------

# Transparent red
myred <- rgb(1, 0, 0, 0.5)

# Set domain
lon_min  <- 3
lon_max  <- 10.5
lat_min  <- +37.5
lat_max  <- +49.5
lon_mask <- lon_min - 1 <= lon & lon <= lon_max + 1
lat_mask <- lat_min - 1 <= lat & lat <= lat_max + 1

# Axis labels
lon_labels <- sprintf("%3.1f", seq(lon_min, lon_max, +0.75))
lon_labels[seq(2,length(lon_labels),2)] <- ""
lat_labels <- sprintf("%3.1f", seq(lat_min, lat_max, +0.75))
lat_labels[seq(2,length(lat_labels),2)] <- ""


# Open PDF device
pdf("fig/figure1b.pdf", width = 40 / 25.4, height = 57.5 / 25.4, pointsize = 8)

# Set margins etc.
par(mar = c(1.5, 2.0, 1.0, 0.75), mgp = c(1, 0.5, 0), tcl = - 1 / 3, ps = 8)

# Blank plot with correct domain
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
  database = "world", 
  fill     = TRUE, 
  col      = "lightgrey", 
  border   = "darkgrey", 
  lwd      = 0.5,
  add      = TRUE 
)

# Add axes
axis(
  side     = 1, 
  at       = seq(from = lon_min, to = lon_max, by = +0.75), 
  labels   = lon_labels, 
  gap.axis = -1, 
  cex.axis = 0.9
)
axis(
  side     = 2, 
  at       = seq(from = lat_min, to = lat_max, by = +0.75), 
  labels   = lat_labels, 
  las      = 1, 
  cex.axis = 0.9
)

# Add figure label
mtext("(b)", 3, line = 0, adj = 0)

# Add TFP mask
.filled.contour(
  x = lon[lon_mask], 
  y = lat[lat_mask], 
  z = tfp[lon_mask,lat_mask], 
  levels = c(min(tfp), -5e-11, max(tfp)), 
  col    = c(myred, NA)
)

# Add contours of TFL = 0
contour(
  x = lon[lon_mask], 
  y = lat[lat_mask], 
  z = frloc[lon_mask,lat_mask], 
  levels     = 0, 
  drawlabels = FALSE, 
  lwd        = 1, 
  add        = TRUE
)

# Add front points
for (front in contour_then_mask) {
  
  # Check if front has any points inside domain
  in_x_domain <- any(lon_min <= front$x & front$x <= lon_max)
  in_y_domain <- any(lat_min <= front$y & front$y <= lat_max)
  
  # Add points if in domain
  if (in_x_domain & in_y_domain) 
    points(front)
  
}

# Close PDF device
dev.off()
