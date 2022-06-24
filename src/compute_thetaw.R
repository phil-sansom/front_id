compute_thetaw <- function (temperature, relative_humidity, pressure) {
  
  # Constants
  p0  <- 1000       # Reference pressure
  Rd  <- 287.04     # Specific gas constant for dry air
  cpd <- 1005.7     # Specific heat capacity of dry air at constant pressure
  epsilon <- 0.622  # Molecular weight ratio of vapor to dry air
  kappad  <- Rd / cpd
  lambda  <- cpd / Rd
  C   <- 273.15
  A   <- 2675
  a   <- 17.67
  b   <- 243.5
  esC <- 6.112
  
  # Bolton (1980) Equation 10
  saturation_vapour_pressure <- esC * 
    exp(a * (temperature - C) / (temperature - C + b))
  vapour_pressure <- relative_humidity / 100 * saturation_vapour_pressure
  # Dew point temperature
  # vapour_pressure <- saturation_vapour_pressure(dew_point), 
  # rearrange Equation 10 above to see
  dew_point <- suppressWarnings(C + b * log(vapour_pressure / esC) / 
                                  (a - log(vapour_pressure / esC)))
  # Mixing ratio
  mixing_ratio <- epsilon * vapour_pressure / (pressure - vapour_pressure)
  
  # Equivalent potential temperature, Bolton (1980)
  # Equation 15
  tl <- suppressWarnings(1 / (1 / (dew_point - 56) + 
                                log(temperature / dew_point) / 800) + 56)
  # Equation 24
  thetal <- temperature * (p0 / (pressure - vapour_pressure))^kappad * 
    (temperature / tl)^(0.28 * mixing_ratio)
  # Equation 39
  thetae <- thetal * 
    exp((3036 / tl - 1.78) * mixing_ratio * (1 + 0.448 * mixing_ratio))
  
  # Wet bulb potential temperature, Davies-Jones (2008), Equation 3.8
  a0 <-   7.101574
  a1 <- -20.68208
  a2 <-  16.11182
  a3 <-   2.574631
  a4 <- - 5.205688
  b1 <- - 3.552497
  b2 <-   3.781782
  b3 <- - 0.6899655
  b4 <- - 0.5929340
  X  <- thetae / C
  X2 <- X * X
  X3 <- X2 * X
  X4 <- X3 * X
  thetaw <- thetae - exp((a0 + a1 * X + a2 * X2 + a3 * X3 + a4 * X4) /
                           (1 + b1 * X + b2 * X2 + b3 * X3 + b4 * X4))
  mask <- thetae < 173.15 | is.na(thetae)
  thetaw[mask] <- thetae[mask]
  
  thetaw
  
} # compute_thetaw
