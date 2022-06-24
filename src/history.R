make_history <- function(command, args) {
  
  time <- format(Sys.time(), "%FT%XZ%z")
  args <- paste(args, collapse = " ")
  
  paste0(time, ": ", command, " ", args)

}
