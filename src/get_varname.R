get_varname = function(nc, target) {
  
  i <- 1
  varname <- nc$var[[i]]$name
  standard_name <- ncatt_get(nc, varname, "standard_name")$value
  while (! standard_name %in% target && i < nc$nvars) {
    
    i <- i + 1
    varname <- nc$var[[i]]$name
    standard_name <- ncatt_get(nc, varname, "standard_name")$value
    
  } # standard_name
  if (! standard_name %in% target) {
    stop(paste0("Unable to determine variable containing ",
                paste(target, collapse = " or "), " in ", 
                nc$filename, ", please specify manually (see --help)."))
  }
  
  varname
  
} # get_varname
