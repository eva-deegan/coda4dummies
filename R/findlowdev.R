#' Save initials of chain with the lowest deviance based on last model run.
#'
#' Function to get new initials based on the chain with the lowest deviance from last run.
#' To be used with get_remove_index.
#'
#' @param to_keep string of variable names
#' @param paramlist list of all parameters tracked. If type is jagsUI, this needs to be in the same order you fed the parameter names into the model
#' @param type rjags or jagsUI. Indicates what object type codaobj is
#' @return saved initials of chain with lowest deviance for next run OR number of lowest deviance chain if just_chain_number=T
#' @export
findlowdev <- function(codaobj, to_keep, paramlist, type, just_chain_number=F){

  # Create a "not in" function using negate from the purrr package
  `%nin%` <- purrr::negate(`%in%`)

  if(type %nin% c("rjags", "jagUI")){
    paste("Please indicate whether this is a rjags or jagsUI samples object")
  }

  jm_coda <- codaobj
    # Save inits based on chains with lowest deviance
    dev_col <- which(colnames(jm_coda[[1]]) == "deviance")
    dev1<- mean(jm_coda[[1]][,dev_col])
    dev2<- mean(jm_coda[[2]][,dev_col])
    dev3<- mean(jm_coda[[3]][,dev_col])
    dev_min <- min(dev1, dev2, dev3)
    if(dev1 == dev_min){
      devin = 1
    } else if(dev2 == dev_min){
      devin = 2
    } else if(dev3 == dev_min){
      devin = 3
    }

    initlow <- devin
    print(paste("chain with lowest deviance: ", initlow, sep=""))

    if(just_chain_number==T){
      return(initlow) #returns the number of the lowest deviance chain
    }

  if(just_chain_number==F){
    remove_vars <- get_remove_index(to_keep, paramlist, type)

    newinits <- initfind(codaobj, OpenBUGS = FALSE)
    saved_state <- removevars(initsin = newinits,
                              variables = remove_vars)

    saved_state <- saved_state[[initlow]]

    return(saved_state) #returns the saved_state initials of the lowest deviance chain
  }

}
