#' Sample Observtions
#'
#' This function will sample n observations, or if n < 1, it will sample the proportion
#' of the total obseravtion specified by n.
#' @param df dataframe to sample
#' @param n integer or numeric < 1 and > 0
#' @return a dataframe with number of observations specified by n, or the proportion of
#' observations specified by n.
#' @export


sample_obs <- function(df, n){
  obs <- length(df[,1])
  if(n < 1){
    n <- round(obs*n)
  }
  samp <- sample(x = 1:obs, size = n)
  df <- df[samp,]

}
