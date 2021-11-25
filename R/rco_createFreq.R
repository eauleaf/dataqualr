#' Takes the raw info for the freq block of the output
#' and puts it in a format usable by the updateReviewObject
#' function
#'
#' @param revObj dataReviewRobject to be updated
#' @param df cleaned data set returned as list element 'coerceData' from function prepareData
#' @return \code{dataReviewRobject}
createFreq <- function(revObj, df){

  # create the frequency list for all dataset columns
  freqObject <- createFreqList(df)
  class(freqObject) <- c("freq")

  return(updateReviewObject(freqObject, revObj))
}



#' Create freq list: a list of data frames. One list item for each original dataset column.
#' The data frame in each list item includes the columns contents sorted by frequency
#'
#' @param df dataframe, output from prepareData
#' @return An dataReviewR freq list
#' @importFrom janitor tabyl
#' @importFrom purrr map
#' @import dplyr
createFreqList <- function(df) {


  # Initialize output list
  out <- list()

  # Short cut
  # If we have no rows, just return the empty list
  if(nrow(df)==0) {
    cat("\n ......data has no rows......")
    return(out)
  }


  # Create frequency info for each variable (janitor::tabyl each column)
  #######################
  cat("\n ......tabyling all data columns......")
  cat(stringr::str_c('\n',names(df)))
  out <- df %>%
    purrr::map(function(x) janitor::tabyl(x) %>% arrange(!is.na(x),desc(n)))
  # out <- df %>%
  #   imap(~ janitor::tabyl(.x) %>% arrange(!is.na(.x),desc(n)) %>% rename(!!.y := .x))
  cat("\n ......tabyled all columns above......")

  return(out)
}
