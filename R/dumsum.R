#' Organize jagsUI output as a dataframe
#'
#' Function to extract posterior means, and 2.5 and 97.5 CI quantiles from jags coda output.
#' It takes a list of variable names and the coda summary.
#' All variables in the list MUST have the same length posterior outputs.
#'
#' @param jagsobj jagsUI or rjags object
#' @param type rjags or jagsUI. Indicates what object type jagsobj is
#' @return A data frame of columns for posterior means, 2.5 %, and 97.5 CI quantiles for each variable
#' @export
dumsum <- function(jagsobj, type){

  # Create a "not in" function using negate from the purrr package
  `%nin%` <- purrr::negate(`%in%`)

  if(type %nin% c("rjags", "jagUI")){
    paste("Please indicate whether this is a rjags or jagsUI samples object")
  }


  if(type == "jagsUI"){

    if(is.null(codaobj$samples)){
      print("This is not a full jagsUI object. If this is just the 'samples' from jagsUI, set 'type' to rjags.")
    }

    jagsui <- jagsobj
    jm_coda <- jagsui$samples # convert to coda form to work with postjags functions

    # Organize the coda object as a dataframe
    df_sum <- coda.fast(jm_coda)
    df_sum <- tibble::rownames_to_column(df_sum, "var")
    df_sum <- df_sum %>% # make index column
      mutate(ID = sub('.*\\[(.*)\\]', '\\1', df_sum$var))
    df_sum$ID <- ifelse(grepl('[[:alpha:]]', df_sum$ID), 1, df_sum$ID) # make ID=1 if there is only 1 instance

    # make a lists of list of indices
    IDlist <- strsplit(df_sum$ID, ",") #temp

    # get number of dims in ID
    counter <- 1
    for(i in 1:length(IDlist)){
      counter <- ifelse(length(IDlist[[i]])>counter, length(IDlist[[i]]), counter)
    }

    # create a character vector of column names based on max dim
    new_columns <- list()
    for(i in 1:counter){
      new_columns[[i]] <- paste("ID",i, sep="")
    }
    new_columns <- as.character(new_columns)

    # for each dimension, create a new column with the correct ID
    df_sum <- df_sum %>%
      tidyr::separate(ID,new_columns,sep=",")

    df_sum <- df_sum %>%
      mutate(var = sub('(.*)\\[.*', '\\1', df_sum$var)) # get rid of numbers in var col

    df_mod <- df_sum %>%
      select("var",starts_with("ID"),"mean","median","pc2.5","pc97.5") %>% #reorder columns, drop ID
      mutate(overlap0 = do.call(c, jagsui$overlap0), gel = do.call(c, jagsui$Rhat))

    df_mod[,2:(counter+1)] <- lapply(2:(counter+1), function(x) as.numeric(df_mod[[x]])) # make appropraite columns numeric

    return(df_mod)
  }

  if(type == "rjags"){
    jm_coda <- jagsobj

    # Organize the coda object as a dataframe
    df_sum <- coda.fast(jm_coda)
    df_sum <- tibble::rownames_to_column(df_sum, "var")
    df_sum <- df_sum %>% # make index column
      mutate(ID = sub('.*\\[(.*)\\]', '\\1', df_sum$var))
    df_sum$ID <- ifelse(grepl('[[:alpha:]]', df_sum$ID), 1, df_sum$ID) # make ID=1 if there is only 1 instance

    # make a lists of list of indices
    IDlist <- strsplit(df_sum$ID, ",") #temp

    # get number of dims in ID
    counter <- 1
    for(i in 1:length(IDlist)){
      counter <- ifelse(length(IDlist[[i]])>counter, length(IDlist[[i]]), counter)
    }

    # create a character vector of column names based on max dim
    new_columns <- list()
    for(i in 1:counter){
      new_columns[[i]] <- paste("ID",i, sep="")
    }
    new_columns <- as.character(new_columns)

    # for each dimension, create a new column with the correct ID
    df_sum <- df_sum %>%
      tidyr::separate(ID,new_columns,sep=",")

    df_sum <- df_sum %>%
      mutate(var = sub('(.*)\\[.*', '\\1', df_sum$var)) # get rid of numbers in var col

    df_mod <- df_sum %>%
      select("var",starts_with("ID"),"mean","median","pc2.5","pc97.5") #%>% #reorder columns, drop ID
    #mutate(overlap0 = do.call(c, jagsui$overlap0), gel = do.call(c, jagsui$Rhat))

    df_mod[,2:(counter+1)] <- lapply(2:(counter+1), function(x) as.numeric(df_mod[[x]])) # make appropraite columns numeric

    return(df_mod)
  }

}
