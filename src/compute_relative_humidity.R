compute_relative_humidity <- function(temperature, specific_humidity, pressure) {
  
  # https://www.ecmwf.int/sites/default/files/elibrary/2016/17117-part-iv-physical-processes.pdf#subsection.7.4.2
  
  a1w <- 611.21
  a3w <- 17.502
  a4w <- 32.19
  a1i <- 611.21
  a3i <- 22.587
  a4i <- -0.7
  t0  <- 273.16
  ti  <- 250.16
  epsilon <- 0.621981
  
  # Vapour pressure
  vapour_pressure <- pressure * specific_humidity / 
    (specific_humidity + epsilon * (1 - specific_humidity))
  
  # Saturated vapor pressure
  saturation_vapour_pressure_water <- 
    a1w * exp(a3w * (temperature - t0) / (temperature - a4w))
  saturation_vapour_pressure_ice <- 
    a1i * exp(a3i * (temperature - t0) / (temperature - a4i))
  not_na <- !is.na(temperature)
  alpha <- array(1, dim(temperature))
  alpha[not_na & temperature <= ti] <- 0
  mask <- not_na & (ti < temperature & temperature < t0)
  alpha[mask] <- ((temperature[mask] - ti) / (t0 - ti))^2
  saturation_vapour_pressure <- alpha * saturation_vapour_pressure_water + 
    (1 - alpha) * saturation_vapour_pressure_ice
  
  # Relative humidity
  relative_humidity <- 100 * vapour_pressure / saturation_vapour_pressure * 
    (pressure - saturation_vapour_pressure) / (pressure - vapour_pressure)
  
} # compute_relative_humidity
