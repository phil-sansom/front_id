classify_fronts <- function(fronts, threshold) {
  
  # Initialize storage
  cold_fronts <- list()
  warm_fronts <- list()
  stat_fronts <- list()
  
  # Initialize counters
  n_cold <- 0
  n_warm <- 0
  n_stat <- 0
  
  # Iterate over fronts
  for (front in fronts) {
    
    # Extract speeds and classify points
    n_points <- length(front$x)
    front_type <- integer(n_points)
    front_speed <- front$frspeed
    front_type[ is.na(front_speed)] <- 0
    front_type[!is.na(front_speed) & front_speed < -threshold] <- 1
    front_type[!is.na(front_speed) & threshold < front_speed] <- 2
    front_type[!is.na(front_speed) & -threshold <= front_speed & 
                 front_speed <= threshold] <- 3
    
    # Split lines into fronts
    current <- 1
    if (n_points > 1) {
      for (i in 1:(n_points - 1)) {
        if (front_type[i] != front_type[i + 1]) {
          if (front_type[i] == 1) {
            n_cold <- n_cold + 1
            cold_fronts[[n_cold]] <- lapply(front, function(x) x[current:i])
          } else if (front_type[i] == 2) {
            n_warm <- n_warm + 1
            warm_fronts[[n_warm]] <- lapply(front, function(x) x[current:i])
          } else if (front_type[i] == 3) {
            n_stat <- n_stat + 1
            stat_fronts[[n_stat]] <- lapply(front, function(x) x[current:i])
          }
          current <- i + 1
        }
      } # i
    } # n_points > 1
    if (front_type[n_points] == 1) {
      n_cold <- n_cold + 1
      cold_fronts[[n_cold]] <- lapply(front, function(x) x[current:n_points])
    } else if (front_type[n_points] == 2) {
      n_warm <- n_warm + 1
      warm_fronts[[n_warm]] <- lapply(front, function(x) x[current:n_points])
    } else if (front_type[n_points] == 3) {
      n_stat <- n_stat + 1
      stat_fronts[[n_stat]] <- lapply(front, function(x) x[current:n_points])
    }
    
  } # front
  
  # Return output
  list(
    cold_fronts = cold_fronts, 
    warm_fronts = warm_fronts,
    stat_fronts = stat_fronts
  )

} # classify_fronts
