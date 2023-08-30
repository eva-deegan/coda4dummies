#' Organize jagsUI output as a dataframe
#'
#' Function to extract posterior means, and 2.5 and 97.5 CI quantiles from jags coda output.
#' It takes a list of variable names and the coda summary.
#' All variables in the list MUST have the same length posterior outputs.
#'
#' @param jagsobj jagsUI or rjags object
#' @param dim max dimension of parameters (between 1-2)
#' @param type rjags or jagsUI. Indicates what object type jagsobj is
#' @return A data frame of columns for posterior means, 2.5 %, and 97.5 CI quantiles for each variable
#' @export
dumsum <- function(jagsobj, dim, type){

  # Create a "not in" function using negate from the purrr package
  `%nin%` <- purrr::negate(`%in%`)

  if(type %nin% c("rjags", "jagUI")){
    paste("Please indicate whether this is a rjags or jagsUI samples object")
  }


  if(type == "jagsUI"){
    jagsui <- jagsobj
    jm_coda <- jagsobj$samples # convert to coda form to work with postjags functions

    # Organize the coda object as a dataframe
    df_sum <- coda.fast(jm_coda)
    df_sum <- rownames_to_column(df_sum, "var")
    df_sum <- df_sum %>% # make index column
      mutate(ID = sub('.*\\[(.*)\\]', '\\1', df_sum$var))
    df_sum <- df_sum %>% # separate index column into 1st and 2nd dimension
      mutate(ID1 = sub('(.*)\\,.*', '\\1', df_sum$ID),
             ID2 = sub('.*\\,(.*)', '\\1', df_sum$ID))
    df_sum$ID2 <- ifelse(!grepl(',', df_sum$ID), NA, df_sum$ID2) # get rid of ID2 if there's no 2nd dimension
    df_sum$ID1 <- ifelse(grepl('[[:alpha:]]', df_sum$ID), 1, df_sum$ID1) # make ID1=1 if there is only 1 instance
    df_sum <- df_sum %>%
      mutate(var = sub('(.*)\\[.*', '\\1', df_sum$var)) # get rid of numbers in var col
    df_mod <- df_sum %>%
      select("var","ID1","ID2","mean","median","pc2.5","pc97.5") %>% #reorder columns, drop ID
      mutate(overlap0 = do.call(c, jagsui$overlap0), gel = do.call(c, jagsui$Rhat))

    return(df_mod)
  }

  if(type == "rjags"){
    jm_coda <- jagsobj

    # Organize the coda object as a dataframe
    df_sum <- coda.fast(jm_coda)
    df_sum <- rownames_to_column(df_sum, "var")
    df_sum <- df_sum %>% # make index column
      mutate(ID = sub('.*\\[(.*)\\]', '\\1', df_sum$var))
    df_sum <- df_sum %>% # separate index column into 1st and 2nd dimension
      mutate(ID1 = sub('(.*)\\,.*', '\\1', df_sum$ID),
             ID2 = sub('.*\\,(.*)', '\\1', df_sum$ID))
    df_sum$ID2 <- ifelse(!grepl(',', df_sum$ID), NA, df_sum$ID2) # get rid of ID2 if there's no 2nd dimension
    df_sum$ID1 <- ifelse(grepl('[[:alpha:]]', df_sum$ID), 1, df_sum$ID1) # make ID1=1 if there is only 1 instance
    df_sum <- df_sum %>%
      mutate(var = sub('(.*)\\[.*', '\\1', df_sum$var)) # get rid of numbers in var col
    df_mod <- df_sum %>%
      select("var","ID1","ID2","mean","median","pc2.5","pc97.5") #%>% #reorder columns, drop ID
    #mutate(overlap0 = do.call(c, jagsui$overlap0), gel = do.call(c, jagsui$Rhat))

    return(df_mod)
  }

}
