#' checkEmpty
#'
#' Checks if a df is actually a single NA, or has no columns
#'
#' @param df a data frame
#' @return None. Stops if empty.
checkEmpty <- function(df) {

  # This was rewritten for speed. We do not want to do is.na() on a massive data frame
  # So this logic ensures we proceed for large data frames without running the is.na
  # step

  if(is.null(ncol(df)) || is.na(ncol(df)) || ncol(df)==0) {
    stop("ERROR : dataframe has no columns")
  }
}



#' CheckNA
#'
#' Checks a data frame is NA - if so, stops
#'
#' @param df A (probable) dataframe
#'
#' @return Nothing. Errors is df is NA

checkNA <- function(df) {
  if(isSingleNA(df)) {
    stop('ERROR : dataframe is empty')
  }
}


#' isSingleNA
#'
#' Boolean function - T if x is a single NA. False otherwise.
#'
#' @param x literally anything
#'
#' @return boolean
isSingleNA <- function(x) {
  if(is.vector(x) && length(x) == 1 && is.na(x)) {
    TRUE
  } else {
    FALSE
  }
}
