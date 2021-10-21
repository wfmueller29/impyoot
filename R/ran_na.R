#' Randomly assign NA's for Certain Variables
#'
#' This function will assign NA's to variables specified for randomly sampled rows. It will
#' also save the old values before randomly assigning NA's
#' @param df the dataframe that we are looking to assing NA's to
#' @param n the number of rows to assign NA's to or the proportion of observations to assign
#' NA's to given as a number < 1.
#' @param vars a character vector of variables that will be assigned NA
#' @return a dataframe with NA values in the var columns for the observations that were sampled.
#' There will also be vars_save columns with the original values before assigning NA's
#' @export


ran_na <- function(df, n, vars){
  obs <- length(df[,1])
  if(n < 1){
    n <- round(obs*n)
  }
  samp <- sample(x = 1:obs, size = n)
  df[,paste0(vars,"_save")] <- df[,vars]
  df[samp, vars] <- NA
  df[samp, "na"] <- 1
  df[is.na(df[, "na"]), "na"] <- 0
  return(df)
}
