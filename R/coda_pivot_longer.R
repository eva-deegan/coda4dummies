#' Convert coda summary output to pivot_longer style data frame columns
#' One column for posterior values, one column for variable names, one column for 2.5 ci, one column for 97.5 ci
#'
#' Function to extract posterior means, and 2.5 and 97.5 CI quantiles from jags coda output.
#' It takes a list of variable names and the coda summary.
#' All variables in the list MUST have the same length posterior outputs.
#' Returns tidyverse pivot_longer format, best for distribution plotting and facet_grids
#'
#' @param
#' @return A data frame of columns for posterior means, 2.5 %, and 97.5 CI quantiles for each variable
#' @export
coda_pivot_longer <- function(var_list, coda_sum, colnams = NULL){

  sum_tb <- coda_sum[["statistics"]]
  quan_tb <- coda_sum[["quantiles"]]

  voi_list <- list()
  column_names <- list()
  j = 1

  for(i in c(1:length(var_list))){

    searchterm <- paste("^", var_list[i], "\\[", sep = "")

    # Check if there is more than instance of the variable or not
    if(length(grep(searchterm, row.names(sum_tb))) == 0){ # if we find nothing
      searchterm <- paste("^", var_list[i], sep = "") # check if there is only one instance and correct the search term
      if(length(grep(searchterm, row.names(sum_tb))) == 0){ # if we still find nothing
        searchterm <- paste(utils::glob2rx(var_list[i]), sep = "") # check if user is using *
        if(length(grep(searchterm, row.names(sum_tb))) == 0){ # if we still find nothing
          print(paste("Warning: ", var_list[i], " not found in coda summary output", sep = ""))
          next
        }
      }
    }

    voi_list[[j]] <- sum_tb[grep(searchterm, row.names(sum_tb)),1]
    voi_list[[j+1]] <- quan_tb[grep(searchterm, row.names(quan_tb)),1]
    voi_list[[j+2]] <- quan_tb[grep(searchterm, row.names(quan_tb)),5]

    if(is.null(colnams)){

      column_names[[j]] <- paste("B_", var_list[i], sep = "")
      column_names[[j+1]] <- paste("ci2.5_", var_list[i], sep = "")
      column_names[[j+2]] <- paste("ci97.5_", var_list[i], sep = "")

    }

    if(!is.null(colnams)){

      column_names[[j]] <- paste(colnams[i], sep = "")
      column_names[[j+1]] <- paste("ci2.5_", colnams[i], sep = "")
      column_names[[j+2]] <- paste("ci97.5_", colnams[i], sep = "")

    }

    j = j + 3

  }
  suppressMessages(df <- dplyr::bind_cols(voi_list))
  colnames(df) <- column_names

  df <- df %>%
    pivot_longer(cols = c(var_list), names_to = "var")

  # pivot_longer ci2.5
  temp <- c()
  if(!is.null(colnams)){
    var_list <- colnams
  }
  col_list <- column_names[grep("2.5",column_names)]
  for( i in c(1:length(var_list))){
    for( j in c(1:nrow(df))){

      # if the var name in the df row matches i
      # then assign the correct ci value to temp
      if(grepl(var_list[i],df$var[j])){
        temp2 <- select(df, ends_with(colnams[i]))[j,]
        temp[[j]] <- as.numeric(temp2)
      }

    }
  }
  df_longer <- df %>%
    mutate(ci2.5 = temp) %>%
    select(-contains(col_list))

  # pivot_longer ci97.5
  temp <- c()
  if(!is.null(colnams)){
    var_list <- colnams
  }
  col_list <- column_names[grep("97.5",column_names)]
  for( i in c(1:length(var_list))){
    for( j in c(1:nrow(df_longer))){

      # if the var name in the df row matches i
      # then assign the correct ci value to temp
      if(grepl(var_list[i],df_longer$var[j])){
        temp2 <- select(df_longer, ends_with(col_list[i]))[j,]
        temp[[j]] <- as.numeric(temp2)
      }

    }
  }
  df_longer <- df_longer %>%
    mutate(ci97.5 = temp) %>%
    select(-contains(col_list))


  return(df_longer)

}
