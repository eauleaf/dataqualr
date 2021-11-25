#' Round all numeric fields in a data frame
#' 
#' @param df A data frame to round
#' @param roundDigits Number of digits to round to
#' @return A rounded data frame

rounddf <- function(df, roundDigits) {
  
  df[sapply(df, is.numeric)] <- lapply(df[sapply(df, is.numeric)], round, roundDigits)
  return(df)
}
