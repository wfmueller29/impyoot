% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute.R
\name{impute}
\alias{impute}
\title{Impute using missForest}
\usage{
impute(df, df_true = NULL, factors = NULL, drop = NULL, ntree = 500, ...)
}
\arguments{
\item{df}{the dataframe that we would like to impute NAs.}

\item{df_true}{optional complete dataframe of the dataframe provided as df argument}

\item{drop}{a character vector of variables that will not be included in the imputation.
These variables will be added to the outputted dataframe, even though they are not inclued in the imputation.}

\item{ntree}{number of trees to grow in each forest. Default is set to 500.}

\item{...}{Other arguments to be passed to missForest imputation, besides ntree}

\item{facotrs}{a character vector of factor variables that we would like specfiy as factors when imputing
These factors variables can be numeric or character variables in the df provided}
}
\value{
A list of output similar to missForest
\item{ximp}{
a dataframe with no missing values. The resulting dataframe will be of the same size as the original df
provided, but with all of the NA's imputed. However, if there are NA's in the drop columns, these values wil
not be imputed.
}
\item{OOBerror}{
estimated OOB imputation error. For the set of continuous
variables in 'xmis' the NRMSE and for the set of categorical variables
the proportion of falsely classified entries is returned. See Details
for the exact definition of these error measures. If 'variablewise'
is set to 'TRUE' then this will be a vector of length 'p' where 'p' is
the number of variables and the entries will be the OOB error for each
variable separately.
}
\item{error}{
true imputation error. This is only available if 'xtrue'
was supplied. The error measures are the same as for 'OOBerror'.
}
}
\description{
This function will allow the user to specify factor variables and variables they do not want
in the imputation. It will then create a data matrix to impute using missForest. It will add drop columns
back after imputation and relabel the factor variables.
}
