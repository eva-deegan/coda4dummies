#' Create new initials based on the initials of the chain with the lowest deviance
#'
#' Function to the chain number with the lowest deviance.
#'
#' @param saved_state saved initials object from the keepvars function
#' @param vary_by the factor you want your new chains to vary around the lowest deviance chain
#' @return new saved_state object of length three, where the second element is your new initials list
#' @export
lowdevrestart <- function(saved_state, vary_by = 10){

  initlow <- saved_state[[3]] # initlow is just the lowest dev chain number

  # take chain with lowest deviance, and make remaining chains vary around it
  saved_state[[2]][[1]] = saved_state[[2]][[initlow]] # Best (low dev) initials for chain 1
  saved_state[[2]][[2]] = lapply(saved_state[[2]][[initlow]],"*",vary_by)
  saved_state[[2]][[3]] = lapply(saved_state[[2]][[initlow]],"/",vary_by)

  return(saved_state)

}
