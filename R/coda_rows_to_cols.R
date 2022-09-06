
# Function to extract posterior means, and 2.5 and 97.5 CI quantiles
# takes a list of variable names and the coda summary
# all variables in the list MUST have the same length posterior outputs

#' Convert coda rows to data frame columns
#'
#' Function to extract posterior means, and 2.5 and 97.5 CI quantiles from jags coda output.
#' It takes a list of variable names and the coda summary.
#' All variables in the list MUST have the same length posterior outputs.
#'
#' @param
#' @return A data frame of columns for posterior means, 2.5 %, and 97.5 CI quantiles for each variable
#' @export
coda_rows_to_cols <- function(var_list, coda_sum){

  sum_tb <- coda_sum[["statistics"]]
  quan_tb <- coda_sum[["quantiles"]]

  voi_list <- list()
  column_names <- list()
  j = 1

  for(i in c(1:length(var_list))){

    searchterm <- paste("^", var_list[i], "\\[", sep = "")

    voi_list[[j]] <- sum_tb[grep(searchterm, row.names(sum_tb)),1]
    voi_list[[j+1]] <- quan_tb[grep(searchterm, row.names(quan_tb)),1]
    voi_list[[j+2]] <- quan_tb[grep(searchterm, row.names(quan_tb)),5]

    column_names[[j]] <- paste("B_", var_list[i], sep = "")
    column_names[[j+1]] <- paste("cred2.5_", var_list[i], sep = "")
    column_names[[j+2]] <- paste("cred97.5_", var_list[i], sep = "")

    j = j + 3

  }
  df <- dplyr::bind_cols(voi_list)
  colnames(df) <- column_names
  return(df)

}
