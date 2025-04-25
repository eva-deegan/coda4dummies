#' Save initials based on last model run.
#'
#' Function to get new initials based on last run.
#' This is essentially a small wrapper function for Michael Fell's removevars function that reverses the input so you just need the names of the variables you want to keep, as opposed to the index of the variables you want to get rid of.
#'
#' @param codaobj jagsUI or rjags object
#' @param to_keep string of variable names
#' @param paramlist list of all parameters tracked. If type is jagsUI, this needs to be in the same order you fed the parameter names into the model
#' @param type rjags or jagsUI. Indicates what object type codaobj is
#' @return saved initials for next run
#' @export
keepvars <- function(codaobj, to_keep, paramlist, type){

  if(!is.null(codaobj$samples)){
    codaobj <- codaobj$samples
  }

  # Create a "not in" function using negate from the purrr package
  `%nin%` <- purrr::negate(`%in%`)

  remove_vars <- get_remove_index(to_keep, paramlist, type)

  newinits <- initfind(codaobj, OpenBUGS = FALSE)

  if(remove_vars!=0){
  saved_state <- removevars(initsin = newinits,
                            variables = remove_vars) 
  }
  if(remove_vars==0){
    saved_state <- newinits
  }

  initlow <- findlowdev(codaobj) # find the lowest deviance chain
  saved_state[[3]] <- initlow
  names(saved_state[[3]]) <- "lowdevchain"

  return(saved_state)

}


#' Get the index of variables you want to remove based on the names of variables you want to keep
#'
#'Function is meant to be used with Michael Fell's removevars function for saving initials
#'
#' @param to_keep string of variable names
#' @param list list of all parameters tracked. If type is jagsUI, this needs to be in the same order you fed the parameter names into the model
#' @param type rjags or jagsUI. Indicates what object type coda object you're using is. This is important because rjags alphabetizes it's parameters whereas jagsUI uses the order you fed the parameters into the model
#' @return A data frame of columns for posterior means, 2.5 %, and 97.5 CI quantiles for each variable
#' @export
get_remove_index <- function(to_keep, list, type){

  # Create a "not in" function using negate from the purrr package
  `%nin%` <- purrr::negate(`%in%`)

  if(type %nin% c("rjags", "jagUI")){
    paste("Please indicate whether this is a rjags or jagsUI samples object")
  }

  if(type == "rjags"){
    list <- list[list != "deviance"] # remove deviance
    list <- sort(list, method = "radix")
    out_list <- c()
    for(j in c(1:length(list))){
      if(list[j] %in% to_keep){
        out_list[j] = NA
      } else{
        out_list[j] = j
      }
    }
    out_list <- out_list[!is.na(out_list)]
    out_list
  }

  if(type == "jagsUI"){
    list <- list[list != "deviance"] # remove deviance
    out_list <- c()
    for(j in c(1:length(list))){
      if(list[j] %in% to_keep){
        out_list[j] = NA
      } else{
        out_list[j] = j
      }
    }
    out_list <- out_list[!is.na(out_list)]
    out_list
  }

}
