#' Impute using missForest
#'
#' This function will allow the user to specify factor variables and variables they do not want
#' in the imputation. It will then create a data matrix to impute using missForest. It will add drop columns
#' back after imputation and relabel the factor variables.
#' @param df the dataframe that we would like to impute NAs
#' @param facotrs a character vector of factor variables that we would like specfiy as factors when imputing
#' These factors variables can be numeric or character variables in the df provided
#' @param drop a character vector of variables that will not be included in the imputation.
#' These variables will be added to the outputted dataframe, even though they are not inclued in the imputation.
#' @param ntree number of trees to grow in each forest. Default is set to 500.
#' @param ... Other arguments to be passed to missForest imputation, besides ntree
#' @return a dataframe with no missing values. The resulting dataframe will be of the same size as the original df
#' provided, but with all of the NA's imputed. However, if there are NA's in the drop columns, these values wil
#' not be imputed.
#' @export


impute <- function(df, factors = NULL, drop = NULL, ntree = 500, ...){
  df_drop <- df[,drop] # save drop variables
  # convert to be factor variables to character, then to factor. This insures no excess factor levels if the variable is originally numeric
  df[,factors] <- lapply(factors, function(factor)as.factor(as.character(df[,factor])))

  dm_prep <- df[!(names(df) %in% drop)] # drop drop variables
  dm_prep <- data.matrix(dm_prep) # create data matrix

  set.seed(365)

  imp <- missForest::missForest(dm_prep, ntree = ntree, ...) # impute with missed forest

  df_imp <- as.data.frame(imp$ximp)
  df_imp <- cbind(df_imp, df_drop) # bind imputed dataframe with dropped columns
  df_imp[,factors] <- lapply(factors, function(fact){ # use old factor levels to relabel factor variables
    factor(as.character(round(df_imp[,fact])), labels = levels(df[,fact]))
  })

  df_imp <- df_imp

  return(df_imp)

}
