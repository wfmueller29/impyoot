#' Impute Last Value Carried Forward
#'
#' This Function will impute the last value carried forward

lvcf <- function(df, var, age){
  df <- df[order(df[,age]),]
  for(row in 1:length(df[,1])){
    if(is.na(df[row,var])){
      if(row == 1){
        value <- df[row,var]
        count <- row
        while(is.na(value)){
          value <- df[count,var]
          count <- count+1
          if(count > length(df[,1])){
            break
          }
        }
        df[row, var] <- value
      } else{
        df[row,var] <- df[row-1,var]
      }
    } else{
      df[row,var] <- df[row, var]
    }
  }
  return(df)
}
