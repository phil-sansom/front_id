# Load libraries
library(ncdf4)
library(maps)

# Load source
source("src/maps_io.R")
source("src/fronts_io.R")
source("src/classify_fronts.R")

# Load maps
nc   <- open_maps("output/contour_then_mask_diagnostics.nc")
dims <- read_dimensions_maps(nc)
lon  <- dims$longitude
lat  <- dims$latitude
maps <- read_maps(nc, t = 1)
close_maps(nc)

# Load fronts
nc <- open_fronts("output/contour_then_mask.nc")
contour_then_mask <- read_fronts(nc, t = 1)
close_fronts(nc)

# Classify fronts
contour_then.mask_classified <- classify_fronts(contour_then_mask, 1.5)

lines_fronts <- function(front, col = 1, lwd = 1) {
  
  mask <- 180 < front$x
  front$x[mask] <- front$x[mask] - 360
  #  lines(front$x, front$y, col = col, lwd = lwd)
  
  if (any(front$x < -150) & any(front$x > 150)) {
    buffer <- front
    mask <- 0 <= buffer$x & buffer$x <= 180
    buffer$x[mask] <- buffer$x[mask] - 360
    lines(buffer$x, buffer$y, col = col, lwd = lwd)
    buffer < -front
    mask<--180 <= buffer$x & buffer$x <= 0
    buffer$x[mask] < -buffer$x[mask] + 360
    lines(buffer$x, buffer$y, col = col, lwd = lwd)
  } else {
    lines(front$x, front$y, col = col, lwd = lwd)
  }
  
}

points_fronts <- function(front, col = 1, pch = 1, cex = 1) {
  
  mask <- 180 < front$x
  front$x[mask] <- front$x[mask] - 360
  
  if (any(front$x < -150) & any(front$x > 150)) {
    buffer <- front
    mask <- 0 <= buffer$x & buffer$x <= 180
    buffer$x[mask] <- buffer$x[mask] - 360
    points(buffer$x, buffer$y, col = col, pch = pch, cex = cex)
    buffer <- front
    mask = -180 <= buffer$x & buffer$x <= 0
    buffer$x[mask] = buffer$x[mask] + 360
    points(buffer$x, buffer$y, col <- col, pch <- pch, cex <- cex)
  } else {
    points(front$x, front$y, col <- col, pch <- pch, cex <- cex)
  }
  
}

# Diagnostics
thetaw <- maps$thetaw
frloc  <- maps$frloc
tfp    <- maps$tfp

# TFP mask
tfp_mask <- (tfp < -5e-11)*1
tfp_mask[tfp_mask == 0] <- NA

# Transparent red
myred <- rgb(1,0,0,0.5)

pdf("fig/figure1b.pdf", width = 40/25.4, height = 57.5/25.4, pointsize = 8)

par(mar = c(1.5,2,1.0,0.75), mgp = c(1,0.5,0), tcl = -1/3, ps = 8)

lon_min <- 3
lon_max <- 10.5
lat_min <- +37.5
lat_max <- +49.5
lon_mask <- lon_min - 1 <= lon & lon <= lon_max + 1
lat_mask <- lat_min - 1 <= lat & lat <= lat_max + 1

lon_labels <- sprintf("%3.1f", seq(lon_min, lon_max, +0.75))
lon_labels[seq(2,length(lon_labels),2)] <- ""
lat_labels <- sprintf("%3.1f", seq(lat_min, lat_max, +0.75))
lat_labels[seq(2,length(lat_labels),2)] <- ""

plot(0, 0, type = "n", asp = 1, xaxs = "i", yaxs = "i",
     xlim = c(lon_min,lon_max), ylim = c(lat_min,lat_max), 
     axes = FALSE, frame.plot = TRUE, ann = FALSE)
map("world", add = TRUE, fill = TRUE, col = "lightgrey", 
    border = "darkgrey", lwd = 0.5)
axis(1, seq(lon_min, lon_max, +0.75), labels = lon_labels, 
     gap.axis = -1, cex.axis = 0.9)
axis(2, seq(lat_min, lat_max, +0.75), labels = lat_labels, 
     las = 1, cex.axis = 0.9)
mtext("(b)", 3, line = 0, adj = 0)
.filled.contour(lon[lon_mask], lat[lat_mask], tfp[lon_mask,lat_mask], 
                c(min(tfp),-5e-11,max(tfp)), c(myred,NA))
contour(lon[lon_mask], lat[lat_mask], frloc[lon_mask,lat_mask], levels = 0, 
        drawlabels = FALSE, lwd = 1, add = TRUE)
for (front in contour_then_mask)
  if (any(lon_min <= front$x & front$x <= lon_max) & 
      any (lat_min <= front$y & front$y <= lat_max))
    points(front)

dev.off()
