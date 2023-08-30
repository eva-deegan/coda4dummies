#' Organize jagsUI output as a dataframe
#'
#' Function to extract posterior means, and 2.5 and 97.5 CI quantiles from jags coda output.
#' It takes a list of variable names and the coda summary.
#' All variables in the list MUST have the same length posterior outputs.
#'
#' @param dfobj Dataframe you want to connect the dates to. Likely output from dumsum
#' @param datevect Vector of dates, doys, or timestamps
#' @param datename What you want the date column to be called
#' @param identifier the optional identifier column name from the dfobj dataframe, how the dates will connect if the variable name is the same with a different index. If not using indices to connect dates, do not assign an identifier.
#' @param varlist Variable names in the dfobj you want to connect dates to
#' @return A data frame with a new column of dates connected to some of the variables only.
#' @export
dateconnect <- function(dfobj, datevect, datename = "date", identifier = NULL, varlist){

  dflistj <- list() # create an empty list for output
  for(j in 1:length(varlist)){
    dfobj2 <- dfobj %>%
      filter(var == varlist[j])

    if(!is.null(identifier)){ # if the variables we want to connect to dates have the same names but are indexed

      IDlist <- unique(as.vector(dfobj2[,identifier]))
      endloop <- length(IDlist)

      dflisti <- list() # create an empty list
      for( i in c(1:endloop)){

        dfobj3 <- dfobj2 %>%
          filter(ID2==as.numeric(IDlist[i]))

        #dfobj3 <- dfobj3[-c(1,2),] # temp for testing because I changed the processing btwn runs to include PAR

        dfobj3[,datename] <- datevect
        dflisti[[i]] <- dfobj3
      }
      dflistj[[j]] <- bind_rows(dflisti)

    }else{ # if the variables we want to connect to dates are just named differently
      dfobj3 <- dfobj2
      dfobj3[,datename] <- datevect
      dflistj[[j]] <- dfobj3
    }
  }

  dfout <- bind_rows(dflistj)

  if(datename %in% colnames(dfobj)){ # if the column name already exists

    dfout[,"tempcol"] <- dfout[,datename] # define a temp column name
    dfout <- dfout[,!names(dfout)==datename] # remove the datename column, or else it messes with joining

    dfoutout <- left_join(dfobj, dfout)

    # when the datename column has an NA, fill it with the tempcol
    for(k in 1:nrow(dfoutout)){
      if(is.na(dfoutout[k,datename])){
        dfoutout[k,datename] <- dfoutout$tempcol[k]
      }
    }

    dfoutout <- dfoutout %>% # delete the temp column
      select(-tempcol)

  }else{
    dfoutout <- left_join(dfobj, dfout)
  }

  return(dfoutout)

}
