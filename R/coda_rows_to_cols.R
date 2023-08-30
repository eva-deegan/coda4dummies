#' Convert coda rows to data frame columns
#'
#' This is a function that only works sometimes. Use dumsum with pivot_wider instead. Function to extract posterior means, and 2.5 and 97.5 CI quantiles from jags coda output.
#' It takes a list of variable names and the coda summary.
#' All variables in the list MUST have the same length posterior outputs.
#'
#' @param
#' @return A data frame of columns for posterior means, 2.5 %, and 97.5 CI quantiles for each variable
#' @export
coda_rows_to_cols <- function(var_list, coda_sum, colnams = NULL){

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
  return(df)

}
